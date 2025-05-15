# pip install accelerate==0.31.0
# pip install transformers==4.41.2
# pip install datasets==2.20.0
# pip install torch==2.3.0+cu121

import pandas as pd
from sklearn.metrics import precision_recall_fscore_support
from sklearn.model_selection import train_test_split
from transformers import DebertaTokenizer, DebertaForSequenceClassification, TrainingArguments, Trainer
from sklearn.preprocessing import LabelEncoder
from datasets import Dataset
import numpy as np
import torch
storage_options = {'User-Agent': 'Mozilla/5.0'}

# save path for gdrive
model_save_path = ''   # put in the link to folder to save model
tokenizer_save_path = '' # put in the link to folder to save tokenizer

acled = pd.read_csv("data/acled_ru_2018_2023_mz.csv",
                    storage_options=storage_options)

man_labeled = pd.read_csv("data/acled_manually_labelled.csv")
man_labeled_subset = man_labeled[['event_id_cnty', 'topic_manual']]

# Merge acled with the subset of man_labeled
acled = pd.merge(acled, man_labeled_subset, on='event_id_cnty', how='left')

# Convert 'event_date' to datetime format
acled['event_date'] = pd.to_datetime(acled['event_date'], format='%d %B %Y')

# Extract year for stratification
acled['year'] = acled['event_date'].dt.year

acled_labels = acled[acled['topic_manual'].notna()]
print("total n: ", acled_labels.shape)

# Label encoding for 'topic_manual'
label_encoder = LabelEncoder()
acled_labels['labels'] = label_encoder.fit_transform(acled_labels['topic_manual'])

# Split the dataset into train, validation, and test sets stratified by year
train_val, test = train_test_split(acled_labels, test_size=0.2, stratify=acled_labels['year'], random_state=123)
train, val = train_test_split(train_val, test_size=0.25, stratify=train_val['year'], random_state=123) 

# Initialize tokenizer
tokenizer = DebertaTokenizer.from_pretrained('microsoft/deberta-base')
max_length = 186 # longest tokenized example in ACLED data

# Tokenizing function
def tokenize_function(examples):
    return tokenizer(examples['notes'], padding="max_length", truncation=True,
                     max_length=max_length)

# Convert DataFrames to Dataset objects
train_dataset = Dataset.from_pandas(train)
val_dataset = Dataset.from_pandas(val)
test_dataset = Dataset.from_pandas(test)

# Tokenize all datasets
train_dataset = train_dataset.map(tokenize_function, batched=True)
val_dataset = val_dataset.map(tokenize_function, batched=True)
test_dataset = test_dataset.map(tokenize_function, batched=True)

NUM_LABELS=len(np.unique(acled_labels['labels']))
# Initialize model
def model_init(seed):
    torch.manual_seed(seed)
    np.random.seed(seed)
    return DebertaForSequenceClassification.from_pretrained('microsoft/deberta-base', num_labels=NUM_LABELS)
 
training_args = TrainingArguments(
    output_dir='./results',
    num_train_epochs=3,
    per_device_train_batch_size=8,
    per_device_eval_batch_size=8,
    learning_rate = 5e-05,
    warmup_steps=500,
    weight_decay=0.01,
    # seed = 42,
    logging_dir='./logs',
    logging_steps=10,
    eval_strategy="steps",  # Evaluate every `eval_steps` steps
    eval_steps=100,  # Number of steps to run evaluation
    save_strategy="steps",  # Save strategy
    save_steps=100,  # Save checkpoint every 100 steps
    load_best_model_at_end=True,  # Load the best model at the end of training
    report_to="none"
)

# Tuning
seeds = [42, 52, 62]
trainers = []

for seed in seeds:
    trainer = Trainer(
        model_init=lambda: model_init(seed),
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=val_dataset,
    )
    trainer.train()
    trainers.append(trainer)

def ensemble_predict(trainers, dataset):
    all_predictions = []

    for trainer in trainers:
        predictions = trainer.predict(dataset)
        all_predictions.append(predictions.predictions)

    avg_predictions = np.mean(all_predictions, axis=0)
    return np.argmax(avg_predictions, axis=1), predictions.label_ids

# Evaluate on the test dataset
ensemble_pred_labels, true_labels = ensemble_predict(trainers, test_dataset)

precision, recall, f1, _ = precision_recall_fscore_support(true_labels, ensemble_pred_labels, average=None, labels=np.unique(true_labels))

precision_micro, recall_micro, f1_micro, _ = precision_recall_fscore_support(true_labels, ensemble_pred_labels, average='micro')
precision_macro, recall_macro, f1_macro, _ = precision_recall_fscore_support(true_labels, ensemble_pred_labels, average='macro')

# Convert metrics to DataFrame
label_names = label_encoder.inverse_transform(np.unique(true_labels))  # Assuming label_encoder is your LabelEncoder instance
metrics_df = pd.DataFrame({
    'Label': np.append(label_names, ['Overall (Micro Avg)', 'Overall (Macro Avg)']),
    'Precision': np.append(np.round(precision, 2), [round(precision_micro, 2), round(precision_macro, 2)]),
    'Recall': np.append(np.round(recall, 2), [round(recall_micro, 2), round(recall_macro, 2)]),
    'F1-Score': np.append(np.round(f1, 2), [round(f1_micro, 2), round(f1_macro, 2)])
})

latex_table = metrics_df.to_latex(index=False, float_format="%.2f", column_format="lccc", caption="Precision, Recall, and F1-Score for each label", label="tab:metrics", header=True, escape=False, bold_rows=True)
print(latex_table)

# save models
for i, trainer in enumerate(trainers):
    model_save_path = os.path.join(model_save_base_path, f'model_{i}')
    trainer.save_model(model_save_path)

tokenizer.save_pretrained(tokenizer_save_path)

##### inferece ### 
# Load the tokenizer and models
tokenizer = DebertaTokenizer.from_pretrained(tokenizer_save_path)

tokenized_notes = tokenizer(list(acled['notes']), padding=True, truncation=True, return_tensors="pt")
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

models = []
for i in range(len(seeds)):
    model_path = os.path.join(model_save_base_path, f'model_{i}')
    model = DebertaForSequenceClassification.from_pretrained(model_path)
    model.to(device)
    models.append(model)

batch_size = 16

# use models for inference
predicted_labels = []

for i in range(0, len(acled['notes']), batch_size):
    batch_notes = list(acled['notes'][i:i+batch_size])
    tokenized_notes = tokenizer(batch_notes, padding=True, truncation=True, return_tensors="pt", max_length=max_length)
    inputs = {key: value.to(device) for key, value in tokenized_notes.items()}

    all_batch_predictions = []

    for model in models:
        model.eval()
        with torch.no_grad():
            outputs = model(**inputs)
            all_batch_predictions.append(outputs.logits.cpu().numpy())

    avg_predictions = np.mean(all_batch_predictions, axis=0)
    batch_predictions = np.argmax(avg_predictions, axis=-1)

    batch_labels = [label_encoder.inverse_transform([label])[0] for label in batch_predictions]
    predicted_labels.extend(batch_labels)
        
acled['pred_labels'] = predicted_labels
# acled.to_csv('data/acled_deberta_preds_17_06_2024.csv')
