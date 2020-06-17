#!/bin/bash

cd ../irmin/real && git pull git@github.com:gs0510/slack_bot_data.git

cd ../../slack_bot && dune exec --  ./src/slack_bot.exe

cd ../irmin/real && git push origin