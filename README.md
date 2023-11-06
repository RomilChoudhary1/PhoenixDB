Phoenix
=====

An OTP application

## Overview

This project has been written to add a Key-Value Store application using Erlang,
LevelDB & ETS as Tech stack. Which supports logging through Folsom and 
inbuilt sharding capabilities. It also has support for automatic cleanup of expired
 data.

### Initial Setup

sudo apt-get install build-essential

sudo apt-get install libssl-dev 

Download and install Erlang OTP(24) : https://stackoverflow.com/questions/44685813/how-do-i-install-a-specific-version-of-erlang-otp

Install snappy using : apt install libsnappy-dev

### Test Application Locally using

    Ensure you already have folder : /opt/phoenix_datastore/leveldb-data 
    Run : sudo ./rebar3 as test shell

### Building & Release

    1) Take Latest Pull in your repository
    2) Inside code repository run: make deb-package
    3) Navigate to the debian package and run : sudo dpkg -i phoenix-0.1.0_x86_64.deb
    4) sudo systemctl daemon-reload
    5) Run or restart app using : sudo service phoenix start / restart

### Important Notes:

    1) Default expired data cleanup for each shard is1 hour which can be changed in phoenix_cleanup_server file.
    2) There are only GET, POST & DELETE API's exposed but various other methods have also been added anyone could easily add new API's for those functionalities.

### API DOCUMENTATION:

    1) Add element to store:
   curl --location 'localhost:8001/phoenix/S3/k5?exp=50' \
   --header 'Content-Type: application/json' \
   --data '1234'

Here field ‘S3’ is the shard or partition key, and ‘k5’ is the key name.
Expiry is set in seconds using query param exp.

    2) Get element from store:
   curl --location --request GET 'localhost:8001/phoenix/S3/k5' \
   --header 'Content-Type: application/json' \
   --data '1234'

    3) Delete element from store:
   curl --location --request DELETE 'localhost:8001/phoenix/S3/k5' \
   --header 'Content-Type: application/json' \
   --data '1234'
