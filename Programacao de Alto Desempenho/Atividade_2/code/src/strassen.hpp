#ifndef STRASSEN_HPP
#define STRASSEN_HPP

#include <vector>
using matrix = std::vector<double>;

void strassen_mult(const matrix &A, const matrix &B, matrix &C, int N);

#endif
