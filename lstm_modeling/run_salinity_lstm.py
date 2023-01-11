# Script to train and predict salinity using an LSTM

# References: 
#     - https://github.com/wcneill/jn-ml-textbook/blob/master/Deep%20Learning/04%20Recurrent%20Networks/pytorch13b_LSTM.ipynb

# Load appropriate libraries/modules
import torch
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler

# Load local functions
import salinity_lstm

##### Load and process the data #####

percent_train = 0.75
percent_valid = 0.25

##### SALINITY TIMESERIES, one site #####

df_all = pd.read_csv('../2_process/out/conus_q_sc_airtemp.csv', 
                     parse_dates=True,
                     dtype={'site_no': str})

df_site = df_all.query("site_no == '01104430'").dropna()
df_site['value'] = df_site.spec_cond
data_salt = df_site.drop(columns=['site_no', 'dateTime', 'airtemp', 'spec_cond'])

# Visualize this data
plt.figure(figsize=[12., 5.])
plt.plot(data_salt.index, data_salt.value, 'b--', label='Spec cond', )

# Splitting data!
from sklearn.model_selection import train_test_split
train_data_salt, valid_data_salt = train_test_split(data_salt, 
                                                    train_size = percent_train,
                                                    test_size = percent_valid,
                                                    random_state = 19,
                                                    shuffle=False)

# Scale/transform columns

# Define the scaling fxns
value_scaler_train_salt = MinMaxScaler(feature_range=(-1, 1))
value_scaler_valid_salt = MinMaxScaler(feature_range=(-1, 1))
q_scaler_train_salt = MinMaxScaler(feature_range=(-1, 1))
q_scaler_valid_salt = MinMaxScaler(feature_range=(-1, 1))

# Then scale the data
train_data_salt.value = value_scaler_train_salt.fit_transform(train_data_salt.value.to_numpy().reshape(-1,1)).reshape(-1)
valid_data_salt.value = value_scaler_valid_salt.fit_transform(valid_data_salt.value.to_numpy().reshape(-1,1)).reshape(-1)
train_data_salt.discharge = q_scaler_train_salt.fit_transform(train_data_salt.discharge.to_numpy().reshape(-1,1)).reshape(-1)
valid_data_salt.discharge = q_scaler_valid_salt.fit_transform(valid_data_salt.discharge.to_numpy().reshape(-1,1)).reshape(-1)

##### Change which dataset will be used #####

# Set more generic names used further below
data = data_salt
train_data_df = train_data_salt.sort_index() # Randomly selected, so need to put back into time order
valid_data_df = valid_data_salt.sort_index()
value_scaler_train = value_scaler_train_salt
value_scaler_valid = value_scaler_valid_salt
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

# Plotting for Salinity example
train_preds, hs = lstm_model(train_data.unsqueeze(0), hs)
train_preds = train_preds[:,vloc].reshape(-1,1) # Only keep the 'value' ones
train_preds = value_scaler_train.inverse_transform(train_preds.detach())
train_time = train_data_df.index

# Get predictions on validation data
valid_preds, hs = lstm_model(valid_x.unsqueeze(0), hs)
valid_preds = valid_preds[:,vloc].reshape(-1,1) # Only keep the 'value' ones
valid_preds = value_scaler_valid.inverse_transform(valid_preds.detach())
valid_time = valid_data_df.index[:-1] # Keep all but the last one to match methods above

##### Plot predictions and actual data #####

# Salinity fig
plt.figure(figsize=[12., 5.])
plt.plot(train_time, train_preds, 'r--', label='Training Predictions', )
plt.plot(valid_time, valid_preds.squeeze(), 'g--', label='Validation Predictions')
plt.plot(data.index, data.value.to_numpy(), label='Actual')
plt.xticks(np.arange(0,145,12))
plt.legend()
