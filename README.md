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

In order to post on Twitter, the default credentials for a Twitter app are required (you can get them here: https://developer.twitter.com/en):

```R
# Create Twitter token
consumer_key1=''
consumer_secret1=''
access_token1=''
access_secret1=''
```

To fully automate the bot, run a cron job in the morning of every day which checks for updates in the database of the RKI (on https://impfdashboard.de/). 
Once the bot has run, it will save the daily update in the database and exit the script during the next cron jobs.

```console
*/10 9-14 * * * /home/vaccinationBot/vaccinationProgress.R >> /home/vaccinationBot/vaccinationProgress.log$
```

