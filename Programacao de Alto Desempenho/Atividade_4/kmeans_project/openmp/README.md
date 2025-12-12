Para compilar

- gcc -O3 -fopenmp ./openmp/kmeans_1d_openmp.c -o ./openmp/kmeans_openmp -lm
- OMP_NUM_THREADS=8 /usr/bin/time -p ./openmp/kmeans_openmp ./data_gen/dados.csv ./data_gen/centroids_in.csv 50 1e-4 ./openmp/assign.csv ./openmp/centroids_out.csv 2> ./openmp/result_time.txt
