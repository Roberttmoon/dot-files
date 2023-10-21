#!/bin/bash
# -*-shell-script-*-
# vim: set filetype=sh :

# dont @ me Catalina
export BASH_SILENCE_DEPRECATION_WARNING=1

source ~/.bashrc

complete -C /usr/local/bin/terraform terraform

complete -C /usr/local/Cellar/packer/1.7.8/libexec/bin/packer packer

complete -C /usr/local/bin/vault vault
export PATH="/usr/local/opt/mysql-client/bin:$PATH"

source /Users/rmoon/.docker/init-bash.sh || true # Added by Docker Desktop

[[ -f ~/.bashrc ]] && source ~/.bashrc # ghcup-env