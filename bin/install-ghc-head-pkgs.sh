#!/bin/bash

# Description: This script builds and installs Haskell libraries specified on
#              the list. It was written to install libraries with GHC HEAD.
#              Source code for the libraries must be placed in subdirectories.
#
# Usage: To build and install libraries run the script without parameters:
#            ./install-ghc-head-pkgs.sh
#        To clean the build artefacts pass the "clean" parameter:
#            ./install-ghc-head-pkgs.sh clean
#
# Author: Jan Stolarek,
#         Institute of Information Technology
#         Lodz University of Technology
#
# License: BSD3


# PKGS contains list of directories containing source code of libraries
#primitive-0.5.0.1 \
#vector-0.10.0.1 \
#dlist-0.5 \
#vector-algorithms-0.5.4.2 \
#bmp-1.2.3.4 \
#ieee754-0.7.3 \
#erf-2.0.0.0 \
#math-functions-0.1.3.0 \
#text-0.11.2.3 \
#hashable-1.2.0.5 \
#attoparsec-0.10.4.0 \
#cereal-0.3.5.2 \
#random-1.0.1.1 \
#QuickCheck-2.5.1.1 \
#parallel-3.2.0.3 \
#mwc-random-0.12.0.1 \
#syb-0.4.0 \
#unordered-containers-0.2.3.0 \
#blaze-builder-0.3.1.0 \
#utf8-string-0.3.7 \
#mtl-2.1.2 \
#hastache-0.5.0 \
#aeson-0.6.1.0 \
#abstract-deque-0.1.6 \
#abstract-par-0.3.1 \
#monad-par-extras-0.3.2 \
#monad-par-0.3.4 \
#statistics-0.10.2.0 \
#parsec-3.1.3 \
#criterion-0.6.2.1 \
#repa-3.2.3.1 \
#repa-io-3.2.3.1 \
#repa-algorithms-3.2.3.1 \
#network-2.4.1.2 \
#HTTP-4000.2.8 \
#zlib-0.5.4.0 \
#unix-2.6.0.1 \
#process-1.1.0.2 \
#ansi-terminal-0.6 \
#ansi-wl-pprint-0.6.6 \
#extensible-exceptions-0.1.1.4 \
#regex-base-0.93.2 \
#regex-posix-0.95.2 \
#regex-compat-0.95.1 \
#hostname-1.0 \
#xml-1.3.13 \
PKGS="\
test-framework-0.8 \
HUnit-1.2.5.2 \
test-framework-hunit-0.3.0 \
QuickCheck-2.5.1.1 \
test-framework-quickcheck2-0.3.0.1 \
"

if [[ $# -gt 1 ]]; then
    echo "Too many parameters"
    exit
elif [[ $# -eq 1 ]]; then
    if [[ $1 == "clean" ]]; then
        echo -n "Cleaning"
        for i in $PKGS
        do
            echo -n "."
            cd $i
            rm -rf dist
            rm -f Setup Setup.o Setup.hi
            cd ..
        done
        echo "done"
    else
        echo "Invalid parameter: $1"
        exit
    fi
else
    for i in $PKGS
    do
        echo "Installing package $i"
        cd $i
        ((if [[ -f Setup.lhs ]]; then ghc Setup.lhs; else ghc Setup.hs; fi) && \
            ./Setup configure --user --enable-shared \
            && ./Setup build && ./Setup install) \
            || exit
        cd ..
    done
fi
