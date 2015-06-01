#!/bin/bash

WHAT=${1:?"Need to specify an action!"}

# http://stackoverflow.com/a/12394284/133514
function recursively_byte_compile() {
    emacs --batch --eval '(byte-recompile-directory "~/.emacs.d")'
}

function byte_compile() {
    emacs -Q --batch -f batch-byte-compile *.el foo/*.el
}


if [ $WHAT = "update-prelude" ];  then
    git up-sub prelude
elif [ $WHAT = "init" ];  then
    git submodules --init --recursive
    git up-sub prelude
    for i in core modules sample utils .projectile init.el; do
        ln -s prelude/$i $i;
    done
    cp -p prelude/.gitignore .gitignore
    cat .gitignore-personal >> .gitignore
elif [ $WHAT = "fork" ]; then
    git co mahmoudimus
    echo 'pushing to mahmoudimus/.emacs.d'
    git push mahmoud mahmoudimus:refs/heads/master
elif [ $WHAT = "master-prelude" ]; then
    git co master
    git fetch origin
    git merge origin/master
else
    echo "Unknown action: $1"
fi
echo "exit code:  $?"
