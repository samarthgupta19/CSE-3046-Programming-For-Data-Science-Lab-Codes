library(jsonlite)
library(httr)
url = paste("https://metals-api.com/api/latest?access_key=6n0m415k6obpeyr3vhvk03r4dif1wlm12sobrx5nu22r6dh4p5sjyedskir9")
data <- fromJSON(url)
print(data)
toJSON(data, pretty = T)
dfs = as.data.frame(data)
dfs
write.table(dfs,"C:\\SPB_Data\\dataset_19BDS0042.csv",append=T,sep=',',row.names=F,col.names=F)

# Batch File Code To Run This Script In Every 5 Minutes :-

# @echo off
# :loop
# C:
# PATH "C:\Program Files\R\R-4.0.3\bin\x64"
# cd C:\SPB_Data
# Rscript A1_19BDS0042.R
# C:\windows\system32\timeout /t 300 /NOBREAK
# goto loop

