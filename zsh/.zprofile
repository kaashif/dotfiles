export PATH=$HOME/.cabal/bin:$HOME/.racket/6.1/bin:/usr/local/jdk-1.7.0/bin:$PATH:$HOME/bin:/sbin:/usr/sbin:/usr/local/sbin:/usr/local/heroku/bin:$HOME/.local/go/bin
export EDITOR="vi"
export GPGKEY=3CA2B425
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export PAGER=less
export GOPATH=$HOME/.local/go
export MPD_HOST=127.0.0.1
export CVSROOT=anoncvs@ftp5.eu.openbsd.org:/cvs
export ALTERNATE_EDITOR=""
export LC_CTYPE="en_GB.UTF-8"
export LC_ALL="en_GB.UTF-8"
export LC_MESSAGES="en_GB.UTF-8"
export LANG="en_GB.UTF-8"
export XDG_DESKTOP_DIR="$HOME"
export XDG_DOWNLOAD_DIR="$HOME"
export DISPLAY=":0"
export SSH_ASKPASS="ssh-askpass"

if [ -z "`pgrep X`" ]; then
	sx
fi
