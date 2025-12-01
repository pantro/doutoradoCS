/* serial_gol.c
   Serial Game of Life as required.
   Compile: gcc -O3 -std=c11 -o serial_gol serial_gol.c
*/
#define _POSIX_C_SOURCE 199309L
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>

#define SRAND_VALUE 1985

/* Default parameters; can override via command line */
int DIM = 2048;
int GENS = 2000;

static inline int idx(int i, int j, int dim){ return i*dim + j; }

int getNeighbors(int *grid, int i, int j, int dim){
    int im1 = (i - 1 + dim) % dim;
    int ip1 = (i + 1) % dim;
    int jm1 = (j - 1 + dim) % dim;
    int jp1 = (j + 1) % dim;
    int cnt = 0;
    cnt += grid[idx(im1,jm1,dim)];
    cnt += grid[idx(im1,j,dim)];
    cnt += grid[idx(im1,jp1,dim)];
    cnt += grid[idx(i,jm1,dim)];
    cnt += grid[idx(i,jp1,dim)];
    cnt += grid[idx(ip1,jm1,dim)];
    cnt += grid[idx(ip1,j,dim)];
    cnt += grid[idx(ip1,jp1,dim)];
    return cnt;
}

double timespec_diff_seconds(struct timespec a, struct timespec b){
    return (a.tv_sec - b.tv_sec) + (a.tv_nsec - b.tv_nsec)*1e-9;
}

int main(int argc, char **argv){
    if(argc >= 2) DIM = atoi(argv[1]);
    if(argc >= 3) GENS = atoi(argv[2]);

    size_t N = (size_t)DIM * DIM;
    int *grid = malloc(N * sizeof(int));
    int *newgrid = malloc(N * sizeof(int));
    if(!grid || !newgrid){ perror("malloc"); return 1; }

    srand(SRAND_VALUE);
    for(int i=0;i<DIM;i++){
        for(int j=0;j<DIM;j++){
            grid[idx(i,j,DIM)] = rand() % 2;
        }
    }

    // compute initial live count
    long long initial_live = 0;
    for(size_t k=0;k<N;k++) initial_live += grid[k];
    printf("Initial live cells: %lld\n", initial_live);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    // Time only loop
    struct timespec loop_start, loop_end;
    clock_gettime(CLOCK_MONOTONIC, &loop_start);
    for(int gen=0; gen<GENS; gen++){
        for(int i=0;i<DIM;i++){
            for(int j=0;j<DIM;j++){
                int neighbors = getNeighbors(grid, i, j, DIM);
                int cur = grid[idx(i,j,DIM)];
                int next = cur;
                if(cur){
                    if(neighbors < 2) next = 0;
                    else if(neighbors == 2 || neighbors == 3) next = 1;
                    else if(neighbors >= 4) next = 0;
                } else {
                    if(neighbors == 3) next = 1;
                    else next = 0;
                }
                newgrid[idx(i,j,DIM)] = next;
            }
        }
        // swap pointers
        int *tmp = grid; grid = newgrid; newgrid = tmp;

        // optional: print some early gens for debug (disabled by default)
        // if(gen < 5) { long long s=0; for(size_t k=0;k<N;k++) s+=grid[k]; printf("Gen %d live=%lld\n", gen+1, s); }
    }
    clock_gettime(CLOCK_MONOTONIC, &loop_end);

    // sum final
    long long final_live = 0;
    for(size_t k=0;k<N;k++) final_live += grid[k];

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double total_sec = timespec_diff_seconds(t1, t0);
    double loop_sec  = timespec_diff_seconds(loop_end, loop_start);

    printf("Final live cells: %lld\n", final_live);
    printf("Total wall-clock time: %.6f s\n", total_sec);
    printf("Loop-only time: %.6f s\n", loop_sec);

    free(grid);
    free(newgrid);
    return 0;
}
