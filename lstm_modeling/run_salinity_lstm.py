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

