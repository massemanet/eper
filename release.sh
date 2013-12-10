#!/bin/sh

[ -f $PWD/src/*.app.src ] && APPSRC=`echo $PWD/src/*.app.src` || exit
OVSN=`grep vsn src/eper.app.src | cut -f2 -d"\""`
NVSN=`echo $OVSN | cut -f1 -d"."`.$((`echo $OVSN | cut -f2 -d"."` + 1))
echo $APPSRC $OVSN"->"$NVSN
sed  s/$OVSN/$NVSN/ < $APPSRC > $$ && mv $$ $APPSRC
git add $APPSRC
git commit -m"v$NVSN"
git log --name-only --no-merges | grep -Ev '^[ ]+$$|git-svn-id' > ChangeLog
echo " Mats Cronqvist <masse@cronqvi.st>" > AUTHORS
git log | grep Author | grep -Evi "vagrant|no author|mats cronqvist" \
  | sort -u | cut -c8- >> AUTHORS
git add ChangeLog AUTHORS
git commit --amend --reuse-message HEAD
git tag -a -m"$NVSN" $NVSN
#git push --tags
