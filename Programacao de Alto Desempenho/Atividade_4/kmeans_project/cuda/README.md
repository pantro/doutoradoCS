Install:
- sudo apt install nvidia-cuda-toolkit

Para compilar:
- nvcc -O3 -gencode arch=compute_60,code=sm_60 ./cuda/kmeans_1d_cuda.cu -o ./cuda/kmeans_cuda
- /usr/bin/time -p ./cuda/kmeans_cuda ./data_gen/dados.csv ./data_gen/centroids_in.csv 50 1e-4 ./cuda/assign.csv ./cuda/centroids_out.csv 2> ./cuda/result_time.txt
