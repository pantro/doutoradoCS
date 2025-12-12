// kmeans_1d_naive.c
// Implementação serial do K-Means 1D.
// Entrada: dados.csv, centroides iniciais, max_iter, eps
// Saída: assign.csv e centroids.csv

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

// Lê vetor de arquivo CSV (um valor por linha)
double* ler_csv_double(const char* nome, long* n) {
    FILE* f = fopen(nome, "r");
    if (!f) { printf("Erro ao abrir %s\n", nome); exit(1); }

    long cap = 1000;
    long count = 0;
    double* v = malloc(cap * sizeof(double));

    while (!feof(f)) {
        if (count == cap) {
            cap *= 2;
            v = realloc(v, cap * sizeof(double));
        }
        if (fscanf(f, "%lf", &v[count]) == 1) count++;
    }
    fclose(f);
    *n = count;
    return v;
}

// Escreve vetor de inteiros
void escrever_csv_int(const char* nome, int* v, long n) {
    FILE* f = fopen(nome, "w");
    for (long i = 0; i < n; i++)
        fprintf(f, "%d\n", v[i]);
    fclose(f);
}

// Escreve vetor de doubles
void escrever_csv_double(const char* nome, double* v, int k) {
    FILE* f = fopen(nome, "w");
    for (int i = 0; i < k; i++)
        fprintf(f, "%.8lf\n", v[i]);
    fclose(f);
}

int main(int argc, char** argv) {
    if (argc < 7) {
        printf("Uso: %s dados.csv centroides.csv max_iter eps assign.csv centroids_out.csv\n", argv[0]);
        return 1;
    }

    const char* arq_dados = argv[1];
    const char* arq_cent = argv[2];
    int max_iter = atoi(argv[3]);
    double eps = atof(argv[4]);
    const char* arq_assign = argv[5];
    const char* arq_cent_out = argv[6];

    long N;
    double* X = ler_csv_double(arq_dados, &N);

    long K_long;
    double* C = ler_csv_double(arq_cent, &K_long);
    int K = (int) K_long;

    int* assign = malloc(N * sizeof(int));
    double* sum = malloc(K * sizeof(double));
    int* cnt = malloc(K * sizeof(int));

    double sse_ant = 1e18;

    for (int iter = 0; iter < max_iter; iter++) {
        for (int c = 0; c < K; c++) sum[c] = 0.0, cnt[c] = 0;

        double sse = 0.0;

        for (long i = 0; i < N; i++) {
            double xi = X[i];
            int best = 0;
            double bestd = fabs(xi - C[0]);

            for (int c = 1; c < K; c++) {
                double d = fabs(xi - C[c]);
                if (d < bestd) {
                    bestd = d;
                    best = c;
                }
            }

            assign[i] = best;
            sum[best] += xi;
            cnt[best]++;

            sse += bestd * bestd;
        }

        // Atualiza centróides
        for (int c = 0; c < K; c++) {
            if (cnt[c] == 0) {
                C[c] = X[rand() % N]; // estratégia simples
            } else {
                C[c] = sum[c] / cnt[c];
            }
        }

        if (fabs(sse - sse_ant) < eps) break;
        sse_ant = sse;
    }

    escrever_csv_int(arq_assign, assign, N);
    escrever_csv_double(arq_cent_out, C, K);

    free(X); free(C); free(assign); free(sum); free(cnt);
    return 0;
}
