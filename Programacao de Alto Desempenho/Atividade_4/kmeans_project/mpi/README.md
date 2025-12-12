Para compilar:
- mpicc -O3 ./mpi/kmeans_1d_mpi.c -o ./mpi/kmeans_mpi -lm
- /usr/bin/time -p mpirun -np 4 ./mpi/kmeans_mpi ./data_gen/dados.csv ./data_gen/centroids_in.csv 50 1e-4 ./mpi/assign.csv ./mpi/centroids_out.csv 2> ./mpi/result_time.txt
