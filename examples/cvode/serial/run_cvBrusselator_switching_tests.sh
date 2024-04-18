#!/bin/bash

for reactor in 0 1 2
do
  for nls in 0 1 2 3 12 13 22 23
  do
    tname="reactor-${reactor}_nls-${nls}"
    echo $tname
    export SUNLOGGER_DEBUG_FILENAME=$tname.debug.log
    ./cvBrusselator $reactor $nls 0 &> $tname.log
    mv ./cvBrusselator_solution.txt $tname.solution.txt
    ./plot_cvBrusselator.py $tname.solution.txt $tname.debug.log
    mv cvBrusselator.png $tname.png
  done
done
