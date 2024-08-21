library(googledrive)

# First step: adjust options
options(gargle_oauth_cache = 'credentials/.secrets')

gargle::gargle_oauth_cache()

drive_auth()
# Second step: authenticate and save the 'token' file (like a password, but it's a file)

list.files('credentials/.secrets/')
