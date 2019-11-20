# COLORS!!!!
NORMAL="\[\033[00m\]"
PURPLE="\[\033[01;34m\]"
BLUE="\[\e[34m\]"
CYAN="\[\e[36m\]"
GREEN="\[\e[32m\]"

# emacs
alias emacs='emacs -nw'

# Brew
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Grep
alias grep='grep -n'
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;35;40'

# Tmux stuff
export TERM="xterm-256color"

# Pandoc stuff

mkmd () { # $1 input org file, $2 output md file
    pandoc -f org -t markdown -s "$1" -o "$2"
}

# Bash Stuff

__ps1_on () {
    items_on="$(__git_ps1)"
    [[ -n "$VIRTUAL_ENV" ]] &&  items_on="$items_on (venv)"
    echo $items_on
}

export PS1="[ ${GREEN}\u${NORMAL}@${PURPLE}\h ${NORMAL}] [ ${CYAN}\w ${NORMAL}] ${BLUE}\$(__ps1_on)\n  ${GREEN}└─ \$${NORMAL}: "

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
alias ls='ls -lghFG'
alias cd..='cd ..'

mkcd() { mkdir -p "$@" && cd "$@"; }

# python stuff
export PATH=/usr/local/bin/ipython:$PATH
export VIRTUAL_ENV_DISABLE_PROMPT=1

vact() {
  source venv/bin/activate
}

vinst() {
  if [ ! -d 'venv' ]; then
    virtualenv venv
    if [ ! -f 'requirements.txt' ]; then
      pip install -r requirements.txt
    fi
  fi
  vact
}

# kube stuff
__kube_ps1 () {
    CONTEXT=$(cat ~/.Kube/config | grep "current-context:" | sed "s/current-context: //")
    if [ -n "$CONTEXT" ]; then
	echo "(k8s: ${CONTEXT})"
    fi
}

# go stuff
alias ggi="go get -insecure"

export GOROOT=/usr/local/go            
export GOPATH=~/Development/gocode
export PATH=$PATH:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin

# rust?
export PATH="$HOME/.cargo/bin:$PATH"