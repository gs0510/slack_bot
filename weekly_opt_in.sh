#!/bin/bash

test_channel=C013R8HP3P1
real_channel=C011F85NCNT

HTTP_CODE=$(curl https://slack.com/api/chat.postMessage -F token=xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD -F channel=${real_channel} -F text="Hi <!here>?, who wants to have a coffee-chat this week? React with a :raised_hand::skin-tone-4:")
echo $HTTP_CODE
