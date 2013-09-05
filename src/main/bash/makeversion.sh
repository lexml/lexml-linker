#!/bin/bash
VERSION=`svnversion | sed -e 's/.*://g'`
cat > src/main/haskell/LexML/Version.hs <<EOF 
module LexML.Version where
 version = "$VERSION"
EOF
