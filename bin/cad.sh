#!/bin/bash

wd=`pwd`
ghc=ghc
options="-fforce-recomp -O0 -dcmm-lint"
filename=$(basename "$1")
filename="${filename%.*}"
dump_dir=$wd/$filename"_dump"

if [[ ! -d $dump_dir ]]; then
  mkdir $dump_dir
fi

compileAndDump() {
  echo -n "Dumping $3... "
  cd $dump_dir
  mkdir $2
  cd $2
  $ghc $options $1 ../../$filename.hs > $dump_dir/$filename.$2
  cd ..
  rm -r $2
  cd ..
  echo "done"
}

compileAndDump -ddump-simpl       hsc     "simplifier"
compileAndDump -ddump-cmm         cmm     "all cmm"
compileAndDump -ddump-cmm-cbe     cbe     "common block elimination"
compileAndDump -ddump-cmm-cfg     cfg     "control flow optimisations"
compileAndDump -ddump-cmm-cps     cps     "CPS"
compileAndDump -ddump-cmm-info    info    "setInfoTableStackMap"
compileAndDump -ddump-cmm-loopify loopify "loopification"
#compileAndDump -ddump-cmm-proc    proc    ""  # does nothing?
#compileAndDump -ddump-cmm-procmap procmap ""  # does nothing?
compileAndDump -ddump-cmm-raw     raw     "raw cmm"
compileAndDump -ddump-cmm-rewrite rewrite "sinking assignments"
compileAndDump -ddump-cmm-sp      sp      "layout stack"
#compileAndDump -ddump-cmm-split   split   ""
compileAndDump -ddump-asm         asm     "NCG assembly"

wait
