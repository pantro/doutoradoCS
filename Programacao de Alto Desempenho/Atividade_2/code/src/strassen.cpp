#include "strassen.hpp"
#include <algorithm>

static void add(const matrix &A, const matrix &B, matrix &R, int N) {
    for (int i = 0; i < N*N; ++i) R[i] = A[i] + B[i];
}
static void sub(const matrix &A, const matrix &B, matrix &R, int N) {
    for (int i = 0; i < N*N; ++i) R[i] = A[i] - B[i];
}

// naive multiply used as base case
static void naive_mult(const matrix &A, const matrix &B, matrix &C, int N) {
    std::fill(C.begin(), C.end(), 0.0);
    for (int i=0;i<N;++i)
        for (int k=0;k<N;++k) {
            double aik = A[i*N + k];
            for (int j=0;j<N;++j)
                C[i*N + j] += aik * B[k*N + j];
        }
}

void strassen_mult(const matrix &A, const matrix &B, matrix &C, int N) {
    // For simplicity, only support N power of two. If not, pad externally before calling.
    if (N <= 64) { // base case threshold
        naive_mult(A,B,C,N);
        return;
    }
    int n2 = N/2;
    auto alloc = [&](int n){ return matrix(n*n);
    };
    // allocate submatrices
    matrix A11(n2*n2), A12(n2*n2), A21(n2*n2), A22(n2*n2);
    matrix B11(n2*n2), B12(n2*n2), B21(n2*n2), B22(n2*n2);
    for (int i=0;i<n2;++i)
        for (int j=0;j<n2;++j) {
            A11[i*n2 + j] = A[i*N + j];
            A12[i*n2 + j] = A[i*N + (j+n2)];
            A21[i*n2 + j] = A[(i+n2)*N + j];
            A22[i*n2 + j] = A[(i+n2)*N + (j+n2)];
            B11[i*n2 + j] = B[i*N + j];
            B12[i*n2 + j] = B[i*N + (j+n2)];
            B21[i*n2 + j] = B[(i+n2)*N + j];
            B22[i*n2 + j] = B[(i+n2)*N + (j+n2)];
        }
    matrix M1(n2*n2), M2(n2*n2), M3(n2*n2), M4(n2*n2), M5(n2*n2), M6(n2*n2), M7(n2*n2);
    matrix T1(n2*n2), T2(n2*n2);
    // M1 = (A11 + A22)*(B11 + B22)
    add(A11,A22,T1,n2); add(B11,B22,T2,n2); strassen_mult(T1,T2,M1,n2);
    // M2 = (A21 + A22)*B11
    add(A21,A22,T1,n2); strassen_mult(T1,B11,M2,n2);
    // M3 = A11*(B12 - B22)
    sub(B12,B22,T2,n2); strassen_mult(A11,T2,M3,n2);
    // M4 = A22*(B21 - B11)
    sub(B21,B11,T2,n2); strassen_mult(A22,T2,M4,n2);
    // M5 = (A11 + A12)*B22
    add(A11,A12,T1,n2); strassen_mult(T1,B22,M5,n2);
    // M6 = (A21 - A11)*(B11 + B12)
    sub(A21,A11,T1,n2); add(B11,B12,T2,n2); strassen_mult(T1,T2,M6,n2);
    // M7 = (A12 - A22)*(B21 + B22)
    sub(A12,A22,T1,n2); add(B21,B22,T2,n2); strassen_mult(T1,T2,M7,n2);
    // C11 = M1 + M4 - M5 + M7
    matrix C11(n2*n2), C12(n2*n2), C21(n2*n2), C22(n2*n2);
    for (int i=0;i<n2*n2;++i)
        C11[i] = M1[i] + M4[i] - M5[i] + M7[i];
    // C12 = M3 + M5
    for (int i=0;i<n2*n2;++i)
        C12[i] = M3[i] + M5[i];
    // C21 = M2 + M4
    for (int i=0;i<n2*n2;++i)
        C21[i] = M2[i] + M4[i];
    // C22 = M1 - M2 + M3 + M6
    for (int i=0;i<n2*n2;++i)
        C22[i] = M1[i] - M2[i] + M3[i] + M6[i];
    // reassemble
    for (int i=0;i<n2;++i)
        for (int j=0;j<n2;++j) {
            C[i*N + j] = C11[i*n2 + j];
            C[i*N + j + n2] = C12[i*n2 + j];
            C[(i+n2)*N + j] = C21[i*n2 + j];
            C[(i+n2)*N + (j+n2)] = C22[i*n2 + j];
        }
}
