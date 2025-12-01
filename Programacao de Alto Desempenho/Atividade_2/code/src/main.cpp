#include <iostream>
#include <vector>
#include <random>
#include <chrono>
#include <cstring>
#include <cmath>
#include <getopt.h>
#include "matmul_orders.hpp"
#include "matmul_blocking.hpp"
#include "strassen.hpp"

// prototype for blas
void blas_dgemm(const std::vector<double>&, const std::vector<double>&, std::vector<double>&, int);

using matrix = std::vector<double>;

static void fill_random(matrix &M, int N, unsigned seed=42) {
    std::mt19937_64 rng(seed);
    std::uniform_real_distribution<double> dist(-1.0,1.0);
    for (auto &x: M) x = dist(rng);
}

static double max_abs_diff(const matrix &A, const matrix &B) {
    double m = 0.0;
    for (size_t i=0;i<A.size();++i) m = std::max(m, std::abs(A[i]-B[i]));
    return m;
}

static void zero(matrix &M) { std::fill(M.begin(), M.end(), 0.0); }

static double run_and_time(void(*f)(const matrix&, const matrix&, matrix&, int), const matrix &A, const matrix &B, matrix &C, int N) {
    zero(C);
    auto t1 = std::chrono::high_resolution_clock::now();
    f(A,B,C,N);
    auto t2 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> dt = t2 - t1;
    return dt.count();
}

int main(int argc, char **argv) {
    int N = 128;
    int reps = 1;
    std::string mode = "all"; // ijk, ikj, jik, jki, kij, kji, block, strassen, blas, all
    int block = 16;
    int opt;
    static struct option long_options[] = {
        {"size", required_argument, 0, 's'},
        {"reps", required_argument, 0, 'r'},
        {"mode", required_argument, 0, 'm'},
        {"block", required_argument, 0, 'b'},
        {0,0,0,0}
    };
    while ((opt = getopt_long(argc, argv, "s:r:m:b:", long_options, NULL)) != -1) {
        switch(opt) {
            case 's': N = atoi(optarg); break;
            case 'r': reps = atoi(optarg); break;
            case 'm': mode = std::string(optarg); break;
            case 'b': block = atoi(optarg); break;
        }
    }
    // if N not power of two, pad for Strassen
    bool need_padding = false;
    int Npad = N;
    auto is_pow2 = [](int x){ return (x & (x-1))==0; };
    if (!is_pow2(N)) {
        int p=1; while (p<N) p<<=1; Npad = p; need_padding = true;
    }
    matrix A(Npad*Npad), B(Npad*Npad), C(Npad*Npad), Cref(Npad*Npad);
    fill_random(A,Npad,123);
    fill_random(B,Npad,456);
    // compute a reference with blas if available, else naive
    zero(Cref);
    if (mode == "all" || mode == "ref") {
        bool use_naive = false;

        if (Npad == N) {
            try {
                blas_dgemm(A, B, Cref, Npad);
            } catch (...) {
                use_naive = true;
            }
        } else {
            use_naive = true;
        }

        if (use_naive) {
            for (int i = 0; i < Npad; ++i)
                for (int k = 0; k < Npad; ++k) {
                    double aik = A[i * Npad + k];
                    for (int j = 0; j < Npad; ++j)
                        Cref[i * Npad + j] += aik * B[k * Npad + j];
                }
        }
    }

    auto run_case = [&](const std::string &case_name, void(*f)(const matrix&, const matrix&, matrix&, int), int useN){
        matrix Cres(Npad*Npad);
        double t = 0.0;
        for (int i=0;i<reps;++i) t += run_and_time(f, A, B, Cres, useN);
        t /= reps;
        double diff = max_abs_diff(Cres, Cref);
        // compute MFLOPS: 2*N^3 ops / (time * 1e6)
        double flops = 2.0 * double(useN) * useN * useN;
        double mflops = (flops / t) / 1e6;

        std::cout << case_name << ", N=" << useN << "," << t << ", MFLOPS=" << mflops << ", max_abs_diff=" << diff << std::endl;
    };

    if (mode=="all" || mode=="ijk") run_case("ijk", matmul_ijk, N);
    if (mode=="all" || mode=="ikj") run_case("ikj", matmul_ikj, N);
    if (mode=="all" || mode=="jik") run_case("jik", matmul_jik, N);
    if (mode=="all" || mode=="jki") run_case("jki", matmul_jki, N);
    if (mode=="all" || mode=="kij") run_case("kij", matmul_kij, N);
    if (mode=="all" || mode=="kji") run_case("kji", matmul_kji, N);
    if (mode=="all" || mode=="block") {
        auto fblock = [&](const matrix &A2, const matrix &B2, matrix &C2, int NN){ matmul_blocked(A2,B2,C2,NN,block); };
        // wrapper to match signature
        auto wrapper = [&](const matrix &A2,const matrix &B2,matrix &C2,int NN){ matmul_blocked(A2,B2,C2,NN,block); };
        // call
        matrix Cres(Npad*Npad);
        double t=0.0;
        for (int i=0;i<reps;++i) {
            zero(Cres);
            auto t1 = std::chrono::high_resolution_clock::now();
            matmul_blocked(A,B,Cres,N,block);
            auto t2 = std::chrono::high_resolution_clock::now();
            t += std::chrono::duration<double>(t2-t1).count();
        }
        t /= reps;
        double flops = 2.0 * double(N) * N * N;
        double mflops = (flops / t) / 1e6;
        double diff = max_abs_diff(Cres, Cref);
        std::cout << "block,B="<<block<<","<<t<<", MFLOPS="<<mflops<<", max_abs_diff="<<diff<<std::endl;
    }
    if (mode=="all" || mode=="strassen") {
        if (need_padding) std::cerr<<"Warning: Strassen requires power-of-two N; padding used to "<<Npad<<std::endl;
        matrix Cres(Npad*Npad);
        double t=0.0;
        for (int i=0;i<reps;++i) {
            zero(Cres);
            auto t1 = std::chrono::high_resolution_clock::now();
            strassen_mult(A,B,Cres,Npad);
            auto t2 = std::chrono::high_resolution_clock::now();
            t += std::chrono::duration<double>(t2-t1).count();
        }
        t /= reps;
        double flops = 2.0 * double(Npad) * Npad * Npad; // approximate
        double mflops = (flops / t) / 1e6;
        double diff = max_abs_diff(Cres, Cref);
        std::cout << "strassen, Npad="<<Npad<<","<<t<<", MFLOPS="<<mflops<<", max_abs_diff="<<diff<<std::endl;
    }
    if (mode=="all" || mode=="blas") {
        matrix Cres(Npad*Npad);
        double t=0.0;
        for (int i=0;i<reps;++i) {
            zero(Cres);
            auto t1 = std::chrono::high_resolution_clock::now();
            blas_dgemm(A,B,Cres,N);
            auto t2 = std::chrono::high_resolution_clock::now();
            t += std::chrono::duration<double>(t2-t1).count();
        }
        t /= reps;
        double flops = 2.0 * double(N) * N * N;
        double mflops = (flops / t) / 1e6;
        double diff = max_abs_diff(Cres, Cref);
        std::cout << "blas, N="<<N<<","<<t<<", MFLOPS="<<mflops<<", max_abs_diff="<<diff<<std::endl;
    }
    return 0;
}
