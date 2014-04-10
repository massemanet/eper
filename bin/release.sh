#!/bin/sh

function usage () {
    echo "$0 major|minor|patch"
    exit
}

[ -f $PWD/src/*.app.src ] && APPSRC=`echo $PWD/src/*.app.src` || usage
if [ -z "$1" ]; then
    size="patch"
else
    size=$1
fi

OVSN=`grep vsn src/*.app.src | cut -f2 -d"\""`
MAJOR=`echo $OVSN | cut -f1 -d"."`
MINOR=`echo $OVSN | cut -f2 -d"."`
PATCH=`echo $OVSN | cut -f3 -d"."`
if [ "$size" == "major" ]; then
    NVSN=$(($MAJOR + 1)).0.0
elif [ "$size" == "minor" ]; then
    NVSN=$MAJOR.$(($MINOR + 1)).0
elif [ "$size" == "patch" ]; then
    NVSN=$MAJOR.$MINOR.$(($PATCH + 1))
else
    usage
fi
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
