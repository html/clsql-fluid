#!/bin/bash

absify () {
    case "$1" in
	/*) echo "$1";;
	*) echo "$PWD/$1";;
    esac
}

set -e
# Only one person uses this, so I'm not too worried about paths.
: ${CLSQL:=/home/sirian/lisp/clsql}
: ${CLSQL_FLUID:=$(dirname "$0")}
CLSQL="$(absify "$CLSQL")"
CLSQL_FLUID="$(absify "$CLSQL_FLUID")"
cd "$CLSQL"
git checkout fluid-pools
LOG="$(git log -1 --pretty=oneline --abbrev-commit)"
cd "$CLSQL_FLUID"
cp -af "$CLSQL/clsql-fluid.asd" .
cp -af "$CLSQL/sql/fluid.lisp" sql/
cat <<EOF >> sql/fluid.lisp
(export 'fluid-database)
(import 'fluid-database '#:clsql)
(export 'fluid-database '#:clsql)
EOF
darcs record -m"Snapshot at $LOG"
