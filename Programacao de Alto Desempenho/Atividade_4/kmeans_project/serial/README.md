Para compilar:
- gcc -O3 -std=c99 ./serial/kmeans_1d_naive.c -o ./serial/kmeans_serial -lm
- /usr/bin/time -p ./serial/kmeans_serial ./data_gen/dados.csv ./data_gen/centroids_in.csv 50 1e-4 ./serial/assign.csv ./serial/centroids_out.csv 2> ./serial/result_time.txt