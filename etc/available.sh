#!/bin/bash

if ./etc/unused.pl | grep "^$1$" -q; then
    echo "Available."
else
    echo "Already in use."
fi
