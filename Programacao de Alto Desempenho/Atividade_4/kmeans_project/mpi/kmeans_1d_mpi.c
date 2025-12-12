// kmeans_1d_mpi.c
// K-Means 1D com MPI
// Uso: mpirun -np P ./kmeans_mpi dados.csv centroides_iniciais.csv max_iter eps assign.csv centroids_out.csv
// Observações:
// - Root (rank 0) lê os arquivos e faz Scatterv dos pontos X.
// - Cada rank calcula localmente assign e sum/cnt. Depois MPI_Reduce para somar sum/cnt e SSE.
// - Root atualiza centróides e faz Broadcast. Ao final, root faz Gatherv de assign para salvar.

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <mpi.h>

double* ler_csv_double_root(const char* nome, long* n) {
    FILE* f = fopen(nome, "r");
    if (!f) { fprintf(stderr,"Erro ao abrir %s\n", nome); MPI_Abort(MPI_COMM_WORLD,1); }
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

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);
    int rank, nproc;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    if (argc < 7) {
        if (rank==0) fprintf(stderr,"Uso: %s dados.csv centroides.csv max_iter eps assign.csv centroids_out.csv\n", argv[0]);
        MPI_Finalize(); return 1;
    }

    const char* arquivo_dados = argv[1];
    const char* arquivo_cent = argv[2];
    int max_iter = atoi(argv[3]);
    double eps = atof(argv[4]);
    const char* arquivo_assign = argv[5];
    const char* arquivo_cent_out = argv[6];

    long N = 0;
    double *X_all = NULL;
    long K_long = 0;
    double *C = NULL;

    if (rank == 0) {
        X_all = ler_csv_double_root(arquivo_dados, &N);
        C = ler_csv_double_root(arquivo_cent, &K_long);
    }

    // broadcast N e K
    MPI_Bcast(&N, 1, MPI_LONG, 0, MPI_COMM_WORLD);
    MPI_Bcast(&K_long, 1, MPI_LONG, 0, MPI_COMM_WORLD);
    int K = (int) K_long;

    // preparar scatterv: cada processo recebe um pedaço de X
    int *sendcounts = NULL;
    int *displs = NULL;
    if (rank == 0) {
        sendcounts = (int*)malloc(nproc * sizeof(int));
        displs = (int*)malloc(nproc * sizeof(int));
        long base = N / nproc;
        long rem = N % nproc;
        int offset = 0;
        for (int p = 0; p < nproc; p++) {
            int cnt = (int)(base + (p < rem ? 1 : 0));
            sendcounts[p] = cnt;
            displs[p] = offset;
            offset += cnt;
        }
    }

    int localN;
    MPI_Scatter(sendcounts, 1, MPI_INT, &localN, 1, MPI_INT, 0, MPI_COMM_WORLD);

    double *X_local = (double*)malloc(localN * sizeof(double));
    // Scatterv the actual data
    MPI_Scatterv(X_all, sendcounts, displs, MPI_DOUBLE, X_local, localN, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    // broadcast centróides iniciais para todos
    if (rank != 0) C = (double*)malloc(K * sizeof(double));
    MPI_Bcast(C, K, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    int *assign_local = (int*)malloc(localN * sizeof(int));
    int *assign_all = NULL;
    if (rank == 0) assign_all = (int*)malloc(N * sizeof(int));

    double *sum = (double*)malloc(K * sizeof(double));
    int *cnt = (int*)malloc(K * sizeof(int));
    double *sum_global = (double*)malloc(K * sizeof(double));
    int *cnt_global = (int*)malloc(K * sizeof(int));

    double sse_prev = 1e18;
    double t0 = MPI_Wtime();

    for (int iter = 0; iter < max_iter; iter++) {
        for (int c = 0; c < K; c++) { sum[c] = 0.0; cnt[c] = 0; }
        double local_sse = 0.0;

        // assignment local
        for (int i = 0; i < localN; i++) {
            double xi = X_local[i];
            int best = 0;
            double bestd = (xi - C[0]) * (xi - C[0]);
            for (int c = 1; c < K; c++) {
                double d = (xi - C[c]) * (xi - C[c]);
                if (d < bestd) { bestd = d; best = c; }
            }
            assign_local[i] = best;
            sum[best] += xi;
            cnt[best] += 1;
            local_sse += bestd;
        }

        // reduzir sum e cnt e sse para root
        MPI_Reduce(sum, sum_global, K, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
        MPI_Reduce(cnt, cnt_global, K, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
        double sse_global;
        MPI_Reduce(&local_sse, &sse_global, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

        // root atualiza centróides
        if (rank == 0) {
            for (int c = 0; c < K; c++) {
                if (cnt_global[c] == 0) {
                    long idx = (long)c % N;
                    C[c] = X_all[idx];
                } else {
                    C[c] = sum_global[c] / (double)cnt_global[c];
                }
            }
        }

        // broadcast novos centróides para todos
        MPI_Bcast(C, K, MPI_DOUBLE, 0, MPI_COMM_WORLD);

        if (rank == 0) {
            if (fabs(sse_global - sse_prev) < eps) {
                // convergiu -> vamos sair (root)
                // gather assigns
                int *recvcounts = sendcounts;
                int *rdispls = displs;
                MPI_Gatherv(assign_local, localN, MPI_INT, assign_all, recvcounts, rdispls, MPI_INT, 0, MPI_COMM_WORLD);
                break;
            }
            sse_prev = sse_global;
        }
        // se não root, ainda precisamos do gather se for última iteração
        if (iter == max_iter - 1) {
            MPI_Gatherv(assign_local, localN, MPI_INT, assign_all, sendcounts, displs, MPI_INT, 0, MPI_COMM_WORLD);
        }
    } // fim iterações

    double t1 = MPI_Wtime();
    if (rank == 0) {
        double elapsed_ms = (t1 - t0) * 1e3;
        printf("Tempo total (ms): %.3f\n", elapsed_ms);
        // salva assign_all e centróides finais
        escrever_csv_int(arquivo_assign, assign_all, N);
        escrever_csv_double(arquivo_cent_out, C, K);
    }

    // libera
    if (rank == 0) { free(X_all); free(assign_all); free(sendcounts); free(displs); }
    free(X_local); free(C); free(assign_local); free(sum); free(cnt); free(sum_global); free(cnt_global);
    MPI_Finalize();
    return 0;
}
