// kmeans_1d_cuda.cu
// K-Means 1D com CUDA
// Uso: ./kmeans_cuda dados.csv centroides_iniciais.csv max_iter eps assign.csv centroids_out.csv
// Observações:
// - Esta implementação usa um kernel que encontra o centróide mais próximo e faz atomicAdds
//   em arrays sum (double) e cnt (int) no device. Depois copia sum/cnt para host para atualizar C.
// - Recomendado GPU com suporte a atomicAdd(double) (compute capability >= 6.0).
// - Mede o tempo total (host -> device -> kernel -> device -> host) em ms.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <cuda.h>

double* ler_csv_double(const char* nome, long* n) {
    FILE* f = fopen(nome, "r");
    if (!f) { fprintf(stderr,"Erro ao abrir %s\n", nome); exit(1); }
    long cap = 1000;
    long count = 0;
    double* v = (double*)malloc(cap * sizeof(double));
    while (!feof(f)) {
        if (count == cap) { cap *= 2; v = (double*)realloc(v, cap * sizeof(double)); }
        if (fscanf(f, "%lf", &v[count]) == 1) count++;
    }
    fclose(f);
    *n = count;
    return v;
}

void escrever_csv_double(const char* nome, double* v, int k) {
    FILE* f = fopen(nome, "w");
    for (int i = 0; i < k; i++) fprintf(f, "%.8lf\n", v[i]);
    fclose(f);
}

void escrever_csv_int(const char* nome, int* v, long n) {
    FILE* f = fopen(nome, "w");
    for (long i = 0; i < n; i++) fprintf(f, "%d\n", v[i]);
    fclose(f);
}

// Kernel: para cada ponto i, encontra o melhor centróide e acumula (atomic) em sum/cnt e sse
__global__ void assign_and_accumulate(const double* X, const double* C, int* assign,
                                      double* sum, int* cnt, double* sse, long N, int K) {
    long i = (long)blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= N) return;
    double xi = X[i];
    int best = 0;
    double bestd = (xi - C[0]) * (xi - C[0]);
    for (int c = 1; c < K; c++) {
        double d = (xi - C[c]) * (xi - C[c]);
        if (d < bestd) { bestd = d; best = c; }
    }
    assign[i] = best;
    // atomic additions: cnt (int) and sum (double) and sse (double)
    atomicAdd(&sum[best], xi);
    atomicAdd(&cnt[best], 1);
    atomicAdd(sse, bestd);
}

double get_time_ms() {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (double)(t.tv_sec * 1e3 + t.tv_nsec / 1e6);
}

int main(int argc, char** argv) {
    if (argc < 7) {
        fprintf(stderr, "Uso: %s dados.csv centroides.csv max_iter eps assign.csv centroids_out.csv\n", argv[0]);
        return 1;
    }
    const char* arquivo_dados = argv[1];
    const char* arquivo_cent = argv[2];
    int max_iter = atoi(argv[3]);
    double eps = atof(argv[4]);
    const char* arquivo_assign = argv[5];
    const char* arquivo_cent_out = argv[6];

    long N;
    double* X = ler_csv_double(arquivo_dados, &N);

    long K_long;
    double* C = ler_csv_double(arquivo_cent, &K_long);
    int K = (int) K_long;

    // Aloca host
    int* assign = (int*)malloc(N * sizeof(int));
    double* sum_host = (double*)malloc(K * sizeof(double));
    int* cnt_host = (int*)malloc(K * sizeof(int));

    // Aloca device
    double *d_X, *d_C, *d_sum, *d_sse;
    int *d_assign, *d_cnt;
    cudaMalloc((void**)&d_X, N * sizeof(double));
    cudaMalloc((void**)&d_C, K * sizeof(double));
    cudaMalloc((void**)&d_assign, N * sizeof(int));
    cudaMalloc((void**)&d_sum, K * sizeof(double));
    cudaMalloc((void**)&d_cnt, K * sizeof(int));
    cudaMalloc((void**)&d_sse, sizeof(double));

    cudaMemcpy(d_X, X, N * sizeof(double), cudaMemcpyHostToDevice);
    cudaMemcpy(d_C, C, K * sizeof(double), cudaMemcpyHostToDevice);

    int threads = 256;
    long blocks = (N + threads - 1) / threads;

    double sse_prev = 1e18;
    double start_all = get_time_ms();

    for (int iter = 0; iter < max_iter; iter++) {
        // zerar d_sum e d_cnt e d_sse
        cudaMemset(d_sum, 0, K * sizeof(double));
        cudaMemset(d_cnt, 0, K * sizeof(int));
        double zero = 0.0;
        cudaMemcpy(d_sse, &zero, sizeof(double), cudaMemcpyHostToDevice);

        // kernel
        assign_and_accumulate<<<blocks, threads>>>(d_X, d_C, d_assign, d_sum, d_cnt, d_sse, N, K);
        cudaDeviceSynchronize();

        // copiar sum/cnt/sse para host
        cudaMemcpy(sum_host, d_sum, K * sizeof(double), cudaMemcpyDeviceToHost);
        cudaMemcpy(cnt_host, d_cnt, K * sizeof(int), cudaMemcpyDeviceToHost);
        double sse_host;
        cudaMemcpy(&sse_host, d_sse, sizeof(double), cudaMemcpyDeviceToHost);

        // atualizar centróides
        for (int c = 0; c < K; c++) {
            if (cnt_host[c] == 0) {
                // estratégia simples: escolher ponto aleatório do host X (usa índice baseado em c)
                long idx = (long)(c) % N;
                C[c] = X[idx];
            } else {
                C[c] = sum_host[c] / (double)cnt_host[c];
            }
        }

        // convergência por SSE
        if (fabs(sse_host - sse_prev) < eps) {
            // copiar assign e centróides e sair
            cudaMemcpy(assign, d_assign, N * sizeof(int), cudaMemcpyDeviceToHost);
            cudaMemcpy(C, d_C, K * sizeof(double), cudaMemcpyDeviceToHost); // C no device não foi atualizada, atualizamos host, logo copiamos host->device abaixo para manter interface
            // atualizar d_C com novos C
            cudaMemcpy(d_C, C, K * sizeof(double), cudaMemcpyHostToDevice);
            break;
        }
        sse_prev = sse_host;

        // copiar novos centróides para device para próxima iteração
        cudaMemcpy(d_C, C, K * sizeof(double), cudaMemcpyHostToDevice);

        // se última iteração: copiar assign para host
        if (iter == max_iter-1) {
            cudaMemcpy(assign, d_assign, N * sizeof(int), cudaMemcpyDeviceToHost);
            cudaMemcpy(C, d_C, K * sizeof(double), cudaMemcpyDeviceToHost);
        }
    }

    double end_all = get_time_ms();
    double elapsed_ms = end_all - start_all;
    printf("Tempo total (ms): %.3f\n", elapsed_ms);

    // salvar assign e centroids
    escrever_csv_int(arquivo_assign, assign, N);
    escrever_csv_double(arquivo_cent_out, C, K);

    // libera memória
    free(X); free(C); free(assign); free(sum_host); free(cnt_host);
    cudaFree(d_X); cudaFree(d_C); cudaFree(d_assign); cudaFree(d_sum); cudaFree(d_cnt); cudaFree(d_sse);
    return 0;
}
