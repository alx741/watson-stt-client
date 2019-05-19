#!/bin/sh

curl -k -X POST --header "Content-Type: application/x-www-form-urlencoded" \
    --header "Accept: application/json" \
    --data-urlencode "grant_type=urn:ibm:params:oauth:grant-type:apikey" \
    --data-urlencode "apikey=$(cat api_key)" \
    "https://iam.bluemix.net/identity/token" \
    | cut -d'"' -f4 > access_token
