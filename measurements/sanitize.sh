#!/bin/bash

awk '{print $8}' $1 | sed -e "s/Mpps//" | tail -n +38 | head -n -1
