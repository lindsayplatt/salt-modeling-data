# Script to train and predict salinity using an LSTM

# References: 
#     - https://github.com/wcneill/jn-ml-textbook/blob/master/Deep%20Learning/04%20Recurrent%20Networks/pytorch13b_LSTM.ipynb

# Load appropriate libraries/modules
import torch
from torch import nn, optim
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns

# Load local functions
import salinity_lstm

##### Load and process the data #####

data = sns.load_dataset("flights")
data.passengers = data.passengers.astype(np.float32) # Convert passenger data to float for PyTorch

# Scale/transform data
scaler = MinMaxScaler(feature_range = -1,1)
data = scaler.fit_transform(data)

# Split data into test and training

train_data = data.passengers[:-12].to_numpy().reshape(-1,1)
valid_data = data.passengers[-12:].to_numpy().reshape(-1,1)

# Convert to a tensor
train_data = torch.tensor(train_data, dtype=torch.float32)
valid_data = torch.tensor(valid_data, dtype=torch.float32)

##### Define model and its parameters #####

input_size = 1
hidden_size = 100
num_layers = 1
output_size = 1

lstm_model = salinityLSTM(input_size, hidden_size, num_layers, output_size)

##### Train the model #####

num_epochs = 500
learning_rate = 0.0005

train(lstm_model, num_epochs, train_data, valid_data, lr=learning_rate)


