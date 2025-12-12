Para compilar:
- nvc -acc -O3 -Minfo=accel ./openacc/kmeans_1d_openacc.c -o ./openacc/kmeans_openacc
- pgcc -acc -fast -Minfo=accel ./openacc/kmeans_1d_openacc.c -o ./openacc/kmeans_openacc

- /usr/bin/time -p ./openacc/kmeans_openacc dados.csv centroides.csv 50 1e-4 ./openacc/assign.csv ./openacc/centroids_out.csv 2> ./mpi/result_time.txt
