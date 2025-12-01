#include <cblas.h>
#include <vector>
using matrix = std::vector<double>;

void blas_dgemm(const matrix &A, const matrix &B, matrix &C, int N) {
    // CblasRowMajor, C = alpha * A * B + beta * C
    double alpha = 1.0;
    double beta = 0.0;
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                N, N, N,
                alpha,
                A.data(), N,
                B.data(), N,
                beta,
                C.data(), N);
}