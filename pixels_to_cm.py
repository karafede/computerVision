


import os
os.chdir('D:/Federico/CityFLows/objectdetection')
os.getcwd()

import numpy as np
import pandas as pd

DF = pd.read_csv("IDs_26Nov2021_piazza_FK.csv")
DF = DF[["X", "Y", "ID", "timestamp"]]

min(DF.X)
min(DF.Y)

max(DF.X)
max(DF.Y)