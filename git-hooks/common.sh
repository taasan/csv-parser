#!/usr/bin/bash

# Trace execution
set -x

STACK_WORKDIR=${PWD}/.stack-work
TMPDIR=$(mktemp -d)

trap "rm -rf '${TMPDIR}'" EXIT

git checkout-index --prefix="${TMPDIR}/" -af

cd "${TMPDIR}"

cp -r "${STACK_WORKDIR}" .

git init
git add .
git commit -m "Git hook ${0}"
make format

CHANGES=$(git status --porcelain --ignored=no)

if [[ ! -z ${CHANGES} ]]
then
    git --no-pager diff
    echo "Source code not properly formatted. Run 'make format'"
    exit 1
fi

make $(basename "${0}")
