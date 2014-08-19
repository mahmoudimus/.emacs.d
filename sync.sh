#!/bin/bash

LOCATION=${1:?"Need to specify fork or source!"}

if [ $LOCATION = "fork" ]; then
    git co mahmoudimus
    echo 'pushing to mahmoudimus/.emacs.d'
    git push mahmoud mahmoudimus:refs/heads/master
else
    git co master
    git fetch origin
    git merge origin/master
fi
echo "exit code:  $?"
