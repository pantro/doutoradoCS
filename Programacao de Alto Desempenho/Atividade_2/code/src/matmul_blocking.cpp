#include "matmul_blocking.hpp"

inline double get2(const matrix &M, int i, int j, int N) { return M[i*N + j]; }
inline void set2(matrix &M, int i, int j, int N, double v) { M[i*N + j] = v; }

void matmul_blocked(const matrix &A, const matrix &B, matrix &C, int N, int Bsize) {
    for (int i0 = 0; i0 < N; i0 += Bsize)
        for (int j0 = 0; j0 < N; j0 += Bsize)
            for (int k0 = 0; k0 < N; k0 += Bsize) {
                int iMax = std::min(i0 + Bsize, N);
                int jMax = std::min(j0 + Bsize, N);
                int kMax = std::min(k0 + Bsize, N);
                for (int i = i0; i < iMax; ++i)
                    for (int k = k0; k < kMax; ++k) {
                        double aik = get2(A,i,k,N);
                        for (int j = j0; j < jMax; ++j)
                            C[i*N + j] += aik * get2(B,k,j,N);
                    }
            }
}