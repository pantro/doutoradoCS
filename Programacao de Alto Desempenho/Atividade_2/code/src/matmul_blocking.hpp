#ifndef MATMUL_BLOCKING_HPP
#define MATMUL_BLOCKING_HPP

#include <vector>
using matrix = std::vector<double>;

void matmul_blocked(const matrix &A, const matrix &B, matrix &C, int N, int Bsize);

#endif
