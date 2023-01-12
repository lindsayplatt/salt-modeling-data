# Script to train an LSTM using example data

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

# Try column for each site with value of interest as the column's values
df_sites.reset_index(inplace=True)
df_sites=df_sites.pivot(index = ['dateTime'], columns = 'site_no', values = 'value')
df_sites = df_sites.dropna() # For now, drop the NAs (will need to learn how to split valid/train appropriately later)

# Show another figure where the NAs are removed
plt.figure(figsize=[12., 5.])
plt.plot(df_sites.index, df_sites['01104430'], 'b--', label='01104430', )
plt.plot(df_sites.index, df_sites['01104480'], 'r--', label='01104480', )
plt.legend()

# Split data into test and training
n_train = math.ceil(len(df_sites)*percent_train)
train_df_sites = df_sites[:n_train]
valid_df_sites = df_sites[n_train:]

plt.figure(figsize=[12., 5.])
plt.plot(train_df_sites.index, train_df_sites['01104430'], 'blue', label='01104430, train', )
plt.plot(valid_df_sites.index, valid_df_sites['01104430'], 'b--', label='01104430, valid', )
plt.plot(train_df_sites.index, train_df_sites['01104480'], 'red', label='01104480, train', )
plt.plot(valid_df_sites.index, valid_df_sites['01104480'], 'r--', label='01104480, valid', )
plt.legend()

# Scale/transform columns

# Define the scaling fxns
value_scaler_train_sites = MinMaxScaler(feature_range=(-1, 1))
value_scaler_valid_sites = MinMaxScaler(feature_range=(-1, 1))

# Then scale the data (use the same scalar for American and Delta)
train_df_sites['01104430'] = value_scaler_train_sites.fit_transform(train_df_sites['01104430'].to_numpy().reshape(-1,1)).reshape(-1)
valid_df_sites['01104430'] = value_scaler_valid_sites.fit_transform(valid_df_sites['01104430'].to_numpy().reshape(-1,1)).reshape(-1)
train_df_sites['01104480'] = value_scaler_train_sites.fit_transform(train_df_sites['01104480'].to_numpy().reshape(-1,1)).reshape(-1)
valid_df_sites['01104480'] = value_scaler_valid_sites.fit_transform(valid_df_sites['01104480'].to_numpy().reshape(-1,1)).reshape(-1)

# Set more generic names used further below
data = df_sites
train_data_df = train_df_sites
valid_data_df = valid_df_sites
value_scaler_train = value_scaler_train_sites
value_scaler_valid = value_scaler_valid_sites
vloc = [data.columns.get_loc('01104430'), data.columns.get_loc('01104480')]

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
train_preds_site1 = train_preds[:,vloc[0]].reshape(-1,1)
train_preds_site1 = value_scaler_train.inverse_transform(train_preds_site1.detach())
train_preds_site2 = train_preds[:,vloc[1]].reshape(-1,1)
train_preds_site2 = value_scaler_train.inverse_transform(train_preds_site2.detach())

# Resetting to be able to use the regular index in plotting because
# the dateTime column causes the second plot values to be squished for
# some reason.
train_out = train_data_df.reset_index(inplace=False) 
train_out['preds_01104430'] = train_preds_site1
train_out['preds_01104480'] = train_preds_site2

valid_preds, hs = lstm_model(valid_x.unsqueeze(0), hs)
valid_preds_site1 = valid_preds[:,vloc[0]].reshape(-1,1)
valid_preds_site1 = value_scaler_valid.inverse_transform(valid_preds_site1.detach())
valid_preds_site2 = valid_preds[:,vloc[1]].reshape(-1,1)
valid_preds_site2 = value_scaler_valid.inverse_transform(valid_preds_site2.detach())

valid_out = valid_data_df.reset_index(inplace=False)[:-1]
valid_out['preds_01104430'] = valid_preds_site1
valid_out['preds_01104480'] = valid_preds_site2

data_out = data.reset_index(inplace=False)

# Adjust the valid time/index used to line up with the appropriate datetimes
valid_time = data_out.index[(data_out['dateTime'] >= min(valid_out.dateTime)) & 
                            (data_out['dateTime'] <= max(valid_out.dateTime))]

##### Plot predictions and actual data #####

fig, (ax1, ax2) = plt.subplots(2, figsize=[12., 5.])
fig.suptitle('Predictions from one-value-column-per-site approach to a multi-site LSTM')
ax1.plot(train_out.index, train_out.preds_01104430, 'r--', label='Training Predictions, 01104430', )
ax1.plot(valid_time, valid_out.preds_01104430, 'g--', label='Validation Predictions, 01104430', )
ax1.plot(data_out.index, data_out['01104430'].to_numpy(), label='Observations, 01104430', )
ax2.plot(train_out.index, train_out.preds_01104480, 'r--', label='Training Predictions, 01104480', )
ax2.plot(valid_time, valid_out.preds_01104480, 'g--', label='Validation Predictions, 01104480')
ax2.plot(data_out.index, data_out['01104480'].to_numpy(), label='Observations, 01104480')
plt.xticks(np.arange(0,145,12))
ax1.legend()
ax2.legend()
