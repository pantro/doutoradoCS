// kmeans_1d_openacc.c
// K-Means 1D com OpenACC
// Uso: ./kmeans_openacc dados.csv centroides_iniciais.csv max_iter eps assign.csv centroids_out.csv
// Estratégia:
// - Diretivas OpenACC para paralelizar o loop de assignment no device.
// - Uso de arrays device sum/cnt com atomic updates (OpenACC atomic) para reduzir por cluster.
// - Mede tempo total (host + transferências) em ms.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

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

double get_time_ms() {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return (double)(t.tv_sec * 1e3 + t.tv_nsec / 1e6);
}

int main(int argc, char** argv) {
    if (argc < 7) {
        fprintf(stderr,"Uso: %s dados.csv centroides.csv max_iter eps assign.csv centroids_out.csv\n", argv[0]);
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
    int K = (int)K_long;

    int* assign = (int*)malloc(N * sizeof(int));
    double* sum = (double*)malloc(K * sizeof(double));
    int* cnt = (int*)malloc(K * sizeof(int));

    double sse_prev = 1e18;
    double t0 = get_time_ms();

    // Copy data to device once
    #pragma acc data copyin(X[0:N], C[0:K]) create(assign[0:N], sum[0:K], cnt[0:K])
    {
        for (int iter = 0; iter < max_iter; iter++) {
            // zerar sum e cnt no device
            #pragma acc parallel loop present(sum[0:K])
            for (int c = 0; c < K; c++) sum[c] = 0.0;
            #pragma acc parallel loop present(cnt[0:K])
            for (int c = 0; c < K; c++) cnt[c] = 0;

            double sse = 0.0;

            // loop de assignment no device: faz atomic updates em sum/cnt e acumula sse por thread
            #pragma acc parallel loop present(X[0:N], C[0:K], assign[0:N], sum[0:K], cnt[0:K]) reduction(+:sse)
            for (long i = 0; i < N; i++) {
                double xi = X[i];
                int best = 0;
                double bestd = (xi - C[0]) * (xi - C[0]);
                for (int c = 1; c < K; c++) {
                    double d = (xi - C[c]) * (xi - C[c]);
                    if (d < bestd) { bestd = d; best = c; }
                }
                assign[i] = best;
                // atomic update em sum[best] e cnt[best]
                #pragma acc atomic update
                sum[best] += xi;
                #pragma acc atomic update
                cnt[best] += 1;
                sse += bestd;
            }

            // copiar sum e cnt para host para atualizar centróides (poderia fazer no device mas para portabilidade deixamos aqui)
            #pragma acc update self(sum[0:K], cnt[0:K])
            // atualizar centróides no host
            for (int c = 0; c < K; c++) {
                if (cnt[c] == 0) {
                    long idx = (long)c % N;
                    C[c] = X[idx];
                } else {
                    C[c] = sum[c] / (double)cnt[c];
                }
            }

            // copiar novas centróides para device
            #pragma acc update device(C[0:K])

            if (fabs(sse - sse_prev) < eps) {
                // copiar assign para host e sair
                #pragma acc update self(assign[0:N])
                goto finished;
            }
            sse_prev = sse;
        }
    }
finished:
    double t1 = get_time_ms();
    double elapsed_ms = t1 - t0;
    printf("Tempo total (ms): %.3f\n", elapsed_ms);

    // salvar assign e centróides (assumimos assign atualizado no host)
    escrever_csv_int(arquivo_assign, assign, N);
    escrever_csv_double(arquivo_cent_out, C, K);

    free(X); free(C); free(assign); free(sum); free(cnt);
    return 0;
}
