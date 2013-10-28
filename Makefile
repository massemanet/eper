.PHONY : changelog

all:
	./rebar compile escriptize

clean:
	./rebar clean

changelog:
	git log --name-only --no-merges \
	  | grep -Ev "^[ ]+$$|git-svn-id" > ChangeLog
	echo " Mats Cronqvist <masse@cronqvi.st>" > AUTHORS
	git log | grep Author | grep -Evi "vagrant|no author|mats cronqvist" \
	  | sort -u | cut -c8- >> AUTHORS
#  this only works correctly if we're ahead of origin/master...
#	git add ChangeLog AUTHORS
#	git commit --amend --reuse-message HEAD
