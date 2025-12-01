#include "matmul_orders.hpp"

inline double get(const matrix &M, int i, int j, int N) { return M[i*N + j]; }
inline void set(matrix &M, int i, int j, int N, double v) { M[i*N + j] = v; }

void matmul_ijk(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int i = 0; i < N; ++i)
        for (int j = 0; j < N; ++j) {
            double sum = 0.0;
            for (int k = 0; k < N; ++k)
                sum += get(A,i,k,N) * get(B,k,j,N);
            set(C,i,j,N,sum);
        }
}

void matmul_ikj(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int i = 0; i < N; ++i)
        for (int k = 0; k < N; ++k) {
            double aik = get(A,i,k,N);
            for (int j = 0; j < N; ++j)
                C[i*N + j] += aik * get(B,k,j,N);
        }
}

void matmul_jik(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int j = 0; j < N; ++j)
        for (int i = 0; i < N; ++i) {
            double sum = 0.0;
            for (int k = 0; k < N; ++k)
                sum += get(A,i,k,N) * get(B,k,j,N);
            set(C,i,j,N,sum);
        }
}

void matmul_jki(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int j = 0; j < N; ++j)
        for (int k = 0; k < N; ++k) {
            double bkj = get(B,k,j,N);
            for (int i = 0; i < N; ++i)
                C[i*N + j] += get(A,i,k,N) * bkj;
        }
}

void matmul_kij(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int k = 0; k < N; ++k)
        for (int i = 0; i < N; ++i) {
            double aik = get(A,i,k,N);
            for (int j = 0; j < N; ++j)
                C[i*N + j] += aik * get(B,k,j,N);
        }
}

void matmul_kji(const matrix &A, const matrix &B, matrix &C, int N) {
    for (int k = 0; k < N; ++k)
        for (int j = 0; j < N; ++j) {
            double bkj = get(B,k,j,N);
            for (int i = 0; i < N; ++i)
                C[i*N + j] += get(A,i,k,N) * bkj;
        }
}