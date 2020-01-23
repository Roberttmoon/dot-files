#!/bin/bash

# This deploys the dot files as symlinks to this
#  directory, it also will move any existing dot
#  files to the same dir with the extension .bak
#  added to the end of them

deploy_dot_file () { # $1 source file $2 symlink file name # $3 symlink filepath
    if [ ! -z "$3" -a -d "$3" ]; then
	mkdir -p "$3"
    fi
    if [ -f "$3$2" -a  -s "$3$2" ]; then
	mv "$3$2" "$3$2.backup"
    fi
    ln -s "$1" "$3$2"
}

# deploy scritp   source file            symlink location  symlink filepath
deploy_dot_file   $(pwd)/bash_profile    ".bash_profile"   "$HOME/"
deploy_dot_file   $(pwd)/bashrc          ".bashrc"         "$HOME/"
deploy_dot_file   $(pwd)/gitignore       ".gitignore"      "$HOME/"
deploy_dot_file   $(pwd)/init.el         "init.el"         "$HOME/.emacs.d/"
deploy_dot_file   $(pwd)/tmux.conf       ".tmux.conf"      "$HOME/"
# deploy_dot_file   $(pwd)/work            ".work"           "$HOME/"         ## dont use this if you already have a .work file
deploy_dot_file   $(pwd)/ispell_english  ".ispell_english" "$HOME/"
