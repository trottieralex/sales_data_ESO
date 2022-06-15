import os
from slpp import slpp as lua
import pandas as pd
from datetime import timezone
import datetime as dt

# Parameters
megaserver = "EU Megaserver"
guild = "Anrchy Aliance"
#folder = "path" # got permission issues... but 

# Get the files to extract
dataFiles = os.listdir()
#dataFiles = [os.path.join(folder, df) for df in dataFiles if "ArkadiusTradeToolsSalesData" in df]
dataFiles = [df for df in dataFiles if "ArkadiusTradeToolsSalesData" in df]


# Open database
database = pd.read_feather("../datasets/all_sales_AA.feather")

print(dataFiles)


# Get data and merge
for file in dataFiles:
    with open(file, "r", encoding="utf8") as ff: # read lua file
        text = ff.read() 
    text = text[text.find("{"):] # remove top line (object=)
    tt = lua.decode(text) # 
    tt = tt[megaserver]['sales'] 

    data = pd.DataFrame.from_dict(tt, orient="index")


    data = data[data.guildName == guild]
    data['dateTime'] = data['timeStamp'].apply(lambda x: dt.datetime.fromtimestamp(x, tz=timezone.utc))

    data = data.drop(['buyerName', 'guildName', 'taxes', 'unitPrice', 'timeStamp'], axis=1)

    data.loc[(data["dateTime"].dt.dayofweek != 1) | (data["dateTime"].dt.time <= dt.time(14, 0, 0)), "week_start"] = data["dateTime"].dt.date - pd.offsets.Week(weekday=1)
    data.loc[(data["dateTime"].dt.dayofweek == 1) & (data["dateTime"].dt.time > dt.time(14, 0, 0)), "week_start"] = data["dateTime"].dt.date


    data = data.reset_index()

    print("###################check")
    print(data.shape)

    
    database = pd.concat([database, data])
    print(database.shape)

    database = database.drop_duplicates()
    print(database.shape)


database.reset_index(drop=True).to_feather("../datasets/all_sales_AA.feather")

# if need csv version:
database.reset_index(drop=True).to_csv("../datasets/all_sales_AA.csv") 