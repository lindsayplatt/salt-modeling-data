
# Script to train an LSTM using example data
# Trying the one-hat encoding approach for different sites

# References: 
#     - https://github.com/wcneill/jn-ml-textbook/blob/master/Deep%20Learning/04%20Recurrent%20Networks/pytorch13b_LSTM.ipynb

# Load appropriate libraries/modules
import torch
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from datetime import datetime
import seaborn as sns
import math

# Load local functions
import salinity_lstm

##### Load and process the data #####

percent_train = 0.75
percent_valid = 0.25

##### FLIGHT DATA EXAMPLE #####

# Create a dummy dataset where there is a categorical value
# 'airline'. Can use this as a way to test getting a gage
# site to work once I move to salinity.
data1 = sns.load_dataset("flights")
data1['airline'] = 'american'
data2 = sns.load_dataset("flights")
data2['airline'] = 'delta'
data2.passengers = data2.passengers + np.random.randint(-50,100,size=len(data2.passengers))

# Visualize this data
plt.figure(figsize=[12., 5.])
plt.plot(data1.index, data1.passengers, 'b--', label='American', )
plt.plot(data2.index, data2.passengers, 'r--', label='Delta', )

data_airlines = pd.concat([data1, data2])
data_airlines['value'] = data_airlines.passengers.astype(np.float32) # Convert passenger data to float for PyTorch
data_airlines.month = data_airlines.month.apply(lambda x: datetime.strptime(x, '%b').month) # Convert months into numeric

# Try one-hot encoding for site ... not great luck
data_airlines = salinity_lstm.onehot_encode_pd(data_airlines, 'airline')
data_airlines = data_airlines.drop(columns=['passengers'])

# Split data into test and training, ensuring both sites
# end up in the training and validation sets
n_all = len(data_airlines); n_each_airline = math.ceil(n_all/2)
n_train = math.ceil(n_each_airline*percent_train)
train_data_airlines = pd.concat([data_airlines[:n_train], 
                                data_airlines[n_each_airline:(n_each_airline+n_train)]])
valid_data_airlines = pd.concat([data_airlines[n_train:n_each_airline], 
                                data_airlines[n_each_airline+n_train:]])

plt.figure(figsize=[12., 5.])
plt.plot(train_data_airlines.index, train_data_airlines.value, 'b--', label='Train', )
plt.plot(valid_data_airlines.index, valid_data_airlines.value, 'g--', label='Valid', )

# Scale/transform columns

# Define the scaling fxns
value_scaler_train_airlines = MinMaxScaler(feature_range=(-1, 1))
value_scaler_valid_airlines = MinMaxScaler(feature_range=(-1, 1))
year_scaler_train_airlines = MinMaxScaler(feature_range=(0, len(data_airlines.year.unique())))
year_scaler_valid_airlines = MinMaxScaler(feature_range=(0, len(data_airlines.year.unique())))

# Then scale the data (use the same scalar for American and Delta)
train_data_airlines.value = value_scaler_train_airlines.fit_transform(train_data_airlines.value.to_numpy().reshape(-1,1)).reshape(-1)
valid_data_airlines.value = value_scaler_valid_airlines.fit_transform(valid_data_airlines.value.to_numpy().reshape(-1,1)).reshape(-1)
train_data_airlines.year = year_scaler_train_airlines.fit_transform(train_data_airlines.year.to_numpy().reshape(-1,1)).reshape(-1)
valid_data_airlines.year = year_scaler_valid_airlines.fit_transform(valid_data_airlines.year.to_numpy().reshape(-1,1)).reshape(-1)

# Set more generic names used further below
data = data_airlines
train_data_df = train_data_airlines
valid_data_df = valid_data_airlines
value_scaler_train = value_scaler_train_airlines
value_scaler_valid = value_scaler_valid_airlines
vloc = data.columns.get_loc('value')

# Convert to a tensor
train_data = torch.tensor(train_data_df.to_numpy(), dtype=torch.float32)
valid_data = torch.tensor(valid_data_df.to_numpy(), dtype=torch.float32)

# Create validation set (I don't really understand why there are separate
# `valid_x` and `valid_y` that are shifted (so valid_x are all the values
# except the last one and valid_y are all the values except the first one
# which means that they both have one less value than the original valid data)
valid_x = valid_data[:-1]
valid_y = valid_data[1:]
valid_data = (valid_x, valid_y)

##### Define model and its parameters #####

input_size = len(data.columns)
hidden_size = 100
num_layers = 1
output_size = len(data.columns) # training the model fails when this is != input_size

lstm_model = salinity_lstm.salinityLSTM(input_size, hidden_size, num_layers, output_size)

##### Train the model #####

num_epochs = 20
learning_rate = 0.0005
print_rate = 10

salinity_lstm.train(lstm_model, num_epochs, train_data, valid_data, lr=learning_rate, print_every=print_rate)

##### Generate predictions using the trained model #####

hs = None

# Get predictions on training data, then valid data

# Plotting for flight example
train_preds, hs = lstm_model(train_data.unsqueeze(0), hs)
train_preds = train_preds[:,vloc].reshape(-1,1)
train_preds = value_scaler_train.inverse_transform(train_preds.detach())
train_time = train_data_df.index

valid_preds, hs = lstm_model(valid_x.unsqueeze(0), hs)
valid_preds = valid_preds[:,vloc].reshape(-1,1)
valid_preds = value_scaler_valid.inverse_transform(valid_preds.detach())
valid_time = valid_data_df.index[:-1] # Keep all but the last one to match methods above

# Separate by airline
train_data_out = train_data_df
train_data_out['preds'] = train_preds
train_preds_american = train_data_out.query('airline_american == 1')
train_preds_delta = train_data_out.query('airline_delta == 1')

valid_data_out = valid_data_df[:-1]
valid_data_out['preds'] = valid_preds
valid_preds_american = valid_data_out.query('airline_american == 1')
valid_preds_delta = valid_data_out.query('airline_delta == 1')

data_out = data
data_out_american = data.query('airline_american == 1')
data_out_delta = data.query('airline_delta == 1')

##### Plot predictions and actual data #####

fig, (ax1, ax2) = plt.subplots(2, figsize=[12., 5.])
fig.suptitle('Vertically stacked subplots')
ax1.plot(train_preds_american.index, train_preds_american.preds, 'r--', label='Training Predictions, American', )
ax2.plot(train_preds_delta.index, train_preds_delta.preds, 'r--', label='Training Predictions, Delta', )
ax1.plot(valid_preds_american.index, valid_preds_american.preds, 'g--', label='Validation Predictions, American', )
ax2.plot(valid_preds_delta.index, valid_preds_delta.preds, 'g--', label='Validation Predictions, Delta', )
ax1.plot(data_out_american.index, data_out_american.value, label='Observations, American')
ax2.plot(data_out_american.index, data_out_american.value, label='Observations, Delta')
plt.xticks(np.arange(0,145,12))
ax1.legend()
ax2.legend()
