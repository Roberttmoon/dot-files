#!/bin/bash

# This deploys the dot files as symlinks to this
#  directory, it also will move any existing dot
#  files to the same dir with the extension .bak
#  added to the end of them

deploy_dot_file () { # $1 source file $2 symlink file name # $3 symlink filepath
    if [ ! -z "$3" -a -d "$3" ]; then
	mkdir -p "$3"
    fi
    if [ -f "$2" -a  -s "$2" ]; then
	mv "$2" "$2.backup"
    fi
    if [ -z "$3" ]; then
	ln -s "$1" "$2"
    else
	ln -s "$1" "$3$2"
    fi
}

# deploy scritp   source file          symlink location       symlink filepath
deploy_dot_file   $(pwd)/bash_profile  "$HOME/.bash_profile"
deploy_dot_file   $(pwd)/bashrc        "$HOME/.bashrc"
deploy_dot_file   $(pwd)/gitignore     "$HOME/.gitignore"
deploy_dot_file   $(pwd)/init.el       init.el               "$HOME/.emacs.d/"
deploy_dot_file   $(pwd)/tmux.conf     "$HOME/.tmux.conf"
deploy_dot_file   $(pwd)/work          "$HOME/.work"
