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

deploy_dot_file bash_profile ~/.bash_profile
deploy_dot_file bashrc ~/.bashrc
deploy_dot_file gitignore ~/.gitignore
deploy_dot_file init.el init.el ~/.emacs.d/
deploy_dot_file tmux.conf ~/.tmux.conf
deploy_dot_file work ~/.work
