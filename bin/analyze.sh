#!/bin/bash

ANALYZE=/5playpen/t-jastol/ghc-validate-lits/nofib/nofib-analyse/nofib-analyse

#echo "Baseline vs. baseline - performance"
#for i in {0..9}; do
#    for ((j=$(($i+1)); j<=9; j++)); do
#      ($ANALYZE baseline-$i.txt baseline-$j.txt | grep "Geometric Mean" | cut -d% -f3) &
#    done
#done
#wait

#echo "Baseline vs. baseline - allocations"
#for i in {0..9}; do
#    for ((j=0; j<=9; j++)); do
#      ($ANALYZE baseline-$i.txt baseline-$j.txt | grep Average | head -2 | tail -1 | cut -b48-) &
#    done
#done
#wait

#echo "Baseline vs. baseline - compilation time"
#for i in {0..9}; do
#    for ((j=0; j<=9; j++)); do
#      ($ANALYZE baseline-$i.txt baseline-$j.txt | tail -n1 | cut -b48- | cut -d% -f1 ) &
#    done
#done
#wait


#for file in "foldRegsDefd-in-conflicts" "inline-glogal-regs" "inline-literals" ; do
#  echo "Baseline vs. $file - performance"
#  for i in {0..9}; do
#    for j in {0..9}; do
#      #echo "$i $j"
#      ($ANALYZE baseline-$i.txt $file-$j.txt | grep "Geometric Mean" | cut -d% -f3) &
#    done
#    wait
#  done
#
#  echo "Baseline vs. $file - allocations"
#  for i in {0..9}; do
#    for j in {0..9}; do
#      #echo "$i $j"
#      ($ANALYZE baseline-$i.txt $file-$j.txt | grep Average | head -2 | tail -1 | cut -b48-) &
#    done
#    wait
#  done
#
#  echo "Baseline vs. $file - compilation time"
#  for i in {0..9}; do
#    for j in {0..9}; do
#      #echo  "$i $j"
#      ($ANALYZE baseline-$i.txt $file-$j.txt | tail -n1 | cut -b48- | cut -d% -f1) &
#    done
#  done
#  wait
#done
