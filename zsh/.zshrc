source $HOME/.zprofile

export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="mytheme"
export CASE_SENSITIVE="true"
export DISABLE_AUTO_UPDATE="true"
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
plugins=(zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh

alias ga="git add -v --all"
alias gcm="git commit -v -m"
alias gd="git diff -v"
alias gp="git push -v"
alias ha="hg add"
alias hr="hg record"
alias hc="hg commit -m"
alias hp="hg push"
alias ssh="TERM=xterm-color ssh"
alias irc="ssh -t earendil.kaashif.co.uk 'tmux attach -t IRC'"
alias oxmail="mutt -F ~/.mutt/oxford"
alias ix="curl -F 'f:1=<-' ix.io"
alias mutt=neomutt

if which doas 2>&1 > /dev/null; then
    alias sudo=doas
fi

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

/usr/games/fortune
