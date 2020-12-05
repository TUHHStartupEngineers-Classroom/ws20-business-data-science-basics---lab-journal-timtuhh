library(httr)
library(jsonlite)  # converts JSON files to R objects

#API to look up which persons are currently in space

res = GET("http://api.open-notify.org/astros.json")

res

data = fromJSON(rawToChar(res$content))

data$people


#API to get time when the ISS flies by over Hamburg

res2 = GET("http://api.open-notify.org/iss-pass.json", query = list(lat = 53.6, lon = 10.0))

data2 = fromJSON(rawToChar(res2$content))

data2$response

# to view it in a normal time it hast to be converted with the anytime package from UNIX time to date time