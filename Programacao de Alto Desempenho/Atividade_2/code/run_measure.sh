#!/bin/bash
# Script para ejecutar mediciones con perf
# Requiere sudo para contadores precisos en algunas distribuciones

SIZES=(128 512 1024)
REPS=3
MODES=(ijk ikj jik jki kij kji block strassen blas)
BLOCKS=(2 4 16 64)
OUT=measurements.csv

echo "mode,N,block,reps,real_time_s,cycles,instructions,L1_dcache_load_misses,LLC_load_misses" > $OUT

for N in "${SIZES[@]}"; do
  for mode in "${MODES[@]}"; do
    if [ "$mode" = "block" ]; then
      for b in "${BLOCKS[@]}"; do
        echo "Running block B=$b N=$N"
        for r in $(seq 1 $REPS); do
          sudo perf stat -x, -e cycles,instructions,L1-dcache-load-misses,LLC-load-misses -- ./bin/matmul_exec --mode block --size $N --reps 1 --block $b 2> /tmp/perf_raw.txt > /tmp/cmd_out.txt
          cycles=$(grep "cycles," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
          instr=$(grep "instructions," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
          l1=$(grep "L1-dcache-load-misses," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
          llc=$(grep "LLC-load-misses," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
          time=$(tail -n1 /tmp/cmd_out.txt | awk -F"," '{print $3}')
          echo "block,$N,$b,1,$time,$cycles,$instr,$l1,$llc" >> $OUT
        done
      done
    else
      echo "Running mode=$mode N=$N"
      for r in $(seq 1 $REPS); do
        sudo perf stat -x, -e cycles,instructions,L1-dcache-load-misses,LLC-load-misses -- ./bin/matmul_exec --mode $mode --size $N --reps 1 2> /tmp/perf_raw.txt > /tmp/cmd_out.txt
        cycles=$(grep "cycles," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
        instr=$(grep "instructions," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
        l1=$(grep "L1-dcache-load-misses," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
        llc=$(grep "LLC-load-misses," /tmp/perf_raw.txt | tail -n1 | awk -F, '{print $1}')
        time=$(tail -n1 /tmp/cmd_out.txt | awk -F"," '{print $3}')
        echo "$mode,$N,0,1,$time,$cycles,$instr,$l1,$llc" >> $OUT
      done
    fi
  done
done

echo "Measurements saved to $OUT"
