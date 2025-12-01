#ifndef MATMUL_ORDERS_HPP
#define MATMUL_ORDERS_HPP

#include <vector>

using matrix = std::vector<double>;

void matmul_ijk(const matrix &A, const matrix &B, matrix &C, int N);
void matmul_ikj(const matrix &A, const matrix &B, matrix &C, int N);
void matmul_jik(const matrix &A, const matrix &B, matrix &C, int N);
void matmul_jki(const matrix &A, const matrix &B, matrix &C, int N);
void matmul_kij(const matrix &A, const matrix &B, matrix &C, int N);
void matmul_kji(const matrix &A, const matrix &B, matrix &C, int N);

#endif
