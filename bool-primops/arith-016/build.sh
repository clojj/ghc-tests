#!/bin/bash

head_ghc="/5playpen/t-jastol/ghc-validate/inplace/bin/ghc-stage2"
patched_ghc="/5playpen/t-jastol/build/inplace/bin/ghc-stage2"

options="-fforce-recomp -msse2 -debug"

rm *.hsc 2>/dev/null
rm *.asm 2>/dev/null
rm *.cmm 2>/dev/null

build () {
  wd=`pwd`
  echo -n "Building $1 (HEAD)... "

  $head_ghc $options $1.hs -ddump-simpl -ddump-asm -ddump-cmm -ddump-to-file -o $1-head

#  $head_ghc $options $1.hs -ddump-simpl -o $1-head > $1-head.hsc || rm $1-head.hsc
#  $head_ghc $options $1.hs -ddump-asm   -o $1-head > $1-head.asm || rm $1-head.asm
#  $head_ghc $options $1.hs -ddump-cmm   -o $1-head > $1-head.cmm || rm $1-head.cmm

  echo "done"

  echo -n "Building $1 (PATCHED)... "
#  $patched_ghc $options $1.hs -ddump-simpl -ddump-asm -ddump-cmm -ddump-to-file -o $1-patched
#  $patched_ghc $options $1.hs -ddump-asm   -o $1-patched -DPATCHED > $1-patched.asm
#  $patched_ghc $options $1.hs -ddump-cmm   -o $1-patched -DPATCHED > $1-patched.cmm

  echo "done"

}

#build "primop"
#build "typeclass"
#build "wrapper"
build "wrapper-double-intern"
build "wrapper-double-extern"
build "wrapper-int-intern"
build "wrapper-int-extern"
build "add-double-intern"
build "add-double-extern"
#build "inlined"

rm *.hi
#rm *.o
