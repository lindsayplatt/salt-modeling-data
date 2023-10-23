
# Script to train an LSTM using example data
# Trying the one-hot encoding approach for different sites

# References: 
#     - https://github.com/wcneill/jn-ml-textbook/blob/master/Deep%20Learning/04%20Recurrent%20Networks/pytorch13b_LSTM.ipynb

# Load appropriate libraries/modules
import torch
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
import math

# Load local functions
import salinity_lstm

##### Load and process the data #####

percent_train = 0.75
percent_valid = 0.25

##### SALINITY TIMESERIES, two sites #####

df_all = pd.read_csv('../2_process/out/conus_q_sc_airtemp.csv', 
                     parse_dates=True,
                     index_col = 'dateTime',
                     dtype={'site_no': str})

df_sites = df_all.query("site_no == ['01104430', '01104480']").dropna()
df_sites['value'] = df_sites.spec_cond
df_sites = df_sites.drop(columns=['spec_cond', 'discharge', 'airtemp'])
# Note that the two site timeseries are different sizes

# Visualize this data
plt.figure(figsize=[12., 5.])
plt.plot(df_sites.query("site_no == '01104430'").index, df_sites.query("site_no == '01104430'").value, 'b--', label='01104430', )
plt.plot(df_sites.query("site_no == '01104480'").index, df_sites.query("site_no == '01104480'").value, 'r--', label='01104480', )
plt.legend()

# Try one-hot encoding for site
df_sites = salinity_lstm.onehot_encode_pd(df_sites, 'site_no')

# Identify dateTimes and their indices prior to splitting into train/validate
# so we can plot later
df_time_site1 = df_sites.query("site_no_01104430 == 1").reset_index().dateTime
df_time_site2 = df_sites.query("site_no_01104480 == 1").reset_index().dateTime

# Split data into test and training, ensuring both sites
# end up in the training and validation sets
n_all = len(df_sites)
n_site1 = len(df_sites.reset_index(inplace=False).query("site_no_01104430 == 1").value)
n_site2 = len(df_sites.reset_index(inplace=False).query("site_no_01104480 == 1").value)
n_train_site1 = math.ceil(n_site1*percent_train)
n_train_site2 = math.ceil(n_site2*percent_train)

train_df_site1 = df_sites.query('site_no_01104430 == 1')[:n_train_site1]
train_df_site2 = df_sites.query('site_no_01104480 == 1')[:n_train_site2]
valid_df_site1 = df_sites.query('site_no_01104430 == 1')[n_train_site1:]
valid_df_site2 = df_sites.query('site_no_01104480 == 1')[n_train_site2:]

train_df_sites = pd.concat([train_df_site1, train_df_site2])
valid_df_sites = pd.concat([valid_df_site1, valid_df_site2])

plt.figure(figsize=[12., 5.])
plt.plot(train_df_site1.index, train_df_site1.value, 'blue', label='01104430, train', )
plt.plot(valid_df_site1.index, valid_df_site1.value, 'b--', label='01104430, valid', )
plt.plot(train_df_site2.index, train_df_site2.value, 'red', label='01104480, train', )
plt.plot(valid_df_site2.index, valid_df_site2.value, 'r--', label='01104480, valid', )
plt.legend()

# Identify the dateTimes/indices of the valid and train splits, used in
# plotting later.
train_time_site1 = df_time_site1[:n_train_site1]
train_time_site2 = df_time_site2[:n_train_site2]
valid_time_site1 = df_time_site1[n_train_site1:]
valid_time_site2 = df_time_site2[n_train_site2:]

# Scale/transform columns

# Define the scaling fxns
value_scaler_train = MinMaxScaler(feature_range=(-1, 1))
value_scaler_valid = MinMaxScaler(feature_range=(-1, 1))

# Then scale the data (use the same scalar for American and Delta)
train_df_sites.value = value_scaler_train.fit_transform(train_df_sites.value.to_numpy().reshape(-1,1)).reshape(-1)
valid_df_sites.value = value_scaler_valid.fit_transform(valid_df_sites.value.to_numpy().reshape(-1,1)).reshape(-1)

# Set more generic names used further below
data = df_sites
train_data_df = train_df_sites
valid_data_df = valid_df_sites
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

train_preds, hs = lstm_model(train_data.unsqueeze(0), hs)
train_preds = train_preds[:,vloc].reshape(-1,1)
train_preds = value_scaler_train.inverse_transform(train_preds.detach())

valid_preds, hs = lstm_model(valid_x.unsqueeze(0), hs)
valid_preds = valid_preds[:,vloc].reshape(-1,1)
valid_preds = value_scaler_valid.inverse_transform(valid_preds.detach())

# Separate by site
train_data_out = train_df_sites
train_data_out['preds'] = train_preds
train_preds_site1 = train_data_out.query('site_no_01104430 == 1')
train_preds_site2 = train_data_out.query('site_no_01104480 == 1')

valid_data_out = valid_df_sites[:-1]
valid_data_out['preds'] = valid_preds
valid_preds_site1 = valid_data_out.query('site_no_01104430 == 1')
valid_preds_site2 = valid_data_out.query('site_no_01104480 == 1')

data_out = data.reset_index()
data_out_site1 = data_out.query('site_no_01104430 == 1')
data_out_site2 = data_out.query('site_no_01104480 == 1')

##### Plot predictions and actual data #####

custom_xlim = (min(data_out.index), max([max(df_time_site1.index), max(df_time_site2.index)]))

fig, (ax1, ax2) = plt.subplots(2, figsize=[12., 5.])
fig.suptitle('Predictions from one-hot encoding approach to a multi-site LSTM')
plt.setp(ax1, xlim=custom_xlim)
plt.setp(ax2, xlim=custom_xlim)
ax1.plot(train_time_site1.index, train_preds_site1.preds, 'r--', label='Training Predictions, site1', )
ax2.plot(train_time_site2.index, train_preds_site2.preds, 'r--', label='Training Predictions, site2', )
ax1.plot(valid_time_site1.index, valid_preds_site1.preds, 'g--', label='Validation Predictions, site1', )
ax2.plot(valid_time_site2[:-1].index, valid_preds_site2.preds, 'g--', label='Validation Predictions, site2', )
ax1.plot(df_time_site1.index, data_out_site1.value, label='Observations, site1')
ax2.plot(df_time_site2.index, data_out_site2.value, label='Observations, site2')
plt.xticks(np.arange(0,145,12))
ax1.legend()
ax2.legend()
