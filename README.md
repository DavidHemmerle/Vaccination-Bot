# Vaccination Bot
Code for the Twitter bot [Impftracker](https://twitter.com/impf_tracker) which keeps track of the current vaccine deliveries as well as the rate of vaccinations.

## Setup

Requires a database (e.g., MySQL) to store daily data. Enter credentials:

```R
con <- dbConnect(RMySQL::MySQL(), 
                dbname = "vac_db", 
                host = "example_host.de",
                # host = "localhost", 
                port = 3306,
                user = "vac_user",
                password = "password")

```

In order to post on Twitter, the default credentials for a Twitter app are required:

```R
# Create Twitter token
consumer_key1=''
consumer_secret1=''
access_token1=''
access_secret1=''
```

