#!/bin/sh
curl https://api.github.com/users/kaashif/repos | jq -r '.[].name' \
 | while read reponame; do
	   if [ -d "${reponame}.git" ]; then
		   cd "${reponame}.git"
		   git fetch
		   cd ..
	   else
		   git clone --bare git://github.com/kaashif/${reponame}.git
	   fi
   done

