source $HOME/.zprofile

export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="mytheme"
export CASE_SENSITIVE="true"
export DISABLE_AUTO_UPDATE="true"
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
plugins=(zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

alias svim="sudo vim"
alias ga="git add -v --all"
alias gcm="git commit -v -m"
alias gd="git diff -v"
alias gp="git push -v"
alias gmail="mutt -F ~/.mutt/gmail"
alias kgmail="mutt -F ~/.mutt/kgmail"
alias mymail="mutt -F ~/.mutt/mymail"
alias ssh="TERM=xterm-color ssh"
alias irc="ssh -t server tmux attach -t IRC"

function cd()
{
    builtin cd $@ && ls
}

function run()
{
    eval "$@" &
}

function em()
{
    emacsclient $(mktemp)
}

if [[ "$1" != "no" ]]; then
	clear
	/usr/games/fortune -s
	printf "\n"
fi
