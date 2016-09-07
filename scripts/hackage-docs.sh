#!/bin/bash

# Run this script in the top-level of your package directory
# (where the .cabal file is) to compile documentation and
# upload it to hackage.

# Requirements:
# cabal-install-1.24 (for --for-hackage)
# haddock 2.17 (for the hyperlinked source)

set -e

VERSION=`grep "^version:" docker.cabal | cut -d " " -f14`
BUILDDIR=dist-newstyle/build/docker-$VERSION
DOCSDIRNAME=docker-$VERSION-docs

cabal act-as-setup -- haddock --builddir=$BUILDDIR --for-hackage --hyperlink-sources
cd $BUILDDIR/doc/html && tar --format=ustar -zcvf $DOCSDIRNAME.tar.gz $DOCSDIRNAME

echo NOW RUN: \n\n
echo cabal upload -d $BUILDDIR/doc/html/$DOCSDIRNAME.tar.gz

