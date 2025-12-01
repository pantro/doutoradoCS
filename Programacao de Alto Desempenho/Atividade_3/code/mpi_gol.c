/* mpi_gol.c
   Parallel Game of Life using MPI.
   Compile: mpicc -O3 -std=c11 -o mpi_gol mpi_gol.c
   Run example: mpirun -np 4 ./mpi_gol 2048 2000
*/
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

#define SRAND_VALUE 1985

static inline int idx(int i, int j, int dim){ return i*dim + j; }

/* get neighbors for a row within local grid with halo: 
   local grid stores (local_rows + 2) rows of length dim:
   row 0 = top halo, rows 1..local_rows = actual rows, row local_rows+1 = bottom halo
*/
int getNeighborsLocal(int *local, int i_local, int j, int dim, int local_rows){
    // i_local in [1..local_rows]
    int cnt = 0;
    int im1 = i_local - 1;
    int ip1 = i_local + 1;
    int jm1 = (j - 1 + dim) % dim;
    int jp1 = (j + 1) % dim;

    cnt += local[idx(im1, jm1, dim)];
    cnt += local[idx(im1, j, dim)];
    cnt += local[idx(im1, jp1, dim)];

    cnt += local[idx(i_local, jm1, dim)];
    cnt += local[idx(i_local, jp1, dim)];

    cnt += local[idx(ip1, jm1, dim)];
    cnt += local[idx(ip1, j, dim)];
    cnt += local[idx(ip1, jp1, dim)];
    return cnt;
}

int main(int argc, char **argv){
    int DIM = 2048;
    int GENS = 2000;
    if(argc >= 2) DIM = atoi(argv[1]);
    if(argc >= 3) GENS = atoi(argv[2]);

    MPI_Init(&argc, &argv);
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    // compute rows per process (distribute remainder to first ranks)
    int base = DIM / size;
    int rem = DIM % size;
    int local_rows = base + (rank < rem ? 1 : 0);

    // displacements for scatter (in rows)
    int *rows_counts = NULL;
    int *rows_displs = NULL;
    if(rank == 0){
        rows_counts = malloc(size * sizeof(int));
        rows_displs = malloc(size * sizeof(int));
        for(int r=0;r<size;r++){
            rows_counts[r] = base + (r < rem ? 1 : 0);
        }
        rows_displs[0] = 0;
        for(int r=1;r<size;r++) rows_displs[r] = rows_displs[r-1] + rows_counts[r-1];
    }

    // allocate local arrays including 2 halo rows
    int row_len = DIM;
    int local_with_halo_rows = local_rows + 2;
    int *local = malloc((size_t)local_with_halo_rows * row_len * sizeof(int));
    int *local_new = malloc((size_t)local_with_halo_rows * row_len * sizeof(int));
    if(!local || !local_new){ perror("malloc"); MPI_Abort(MPI_COMM_WORLD,1); }

    // Root initializes full grid and scatters
    int *full_grid = NULL;
    if(rank == 0){
        size_t N = (size_t)DIM * DIM;
        full_grid = malloc(N * sizeof(int));
        if(!full_grid){ perror("malloc full"); MPI_Abort(MPI_COMM_WORLD,1); }
        srand(SRAND_VALUE);
        for(int i=0;i<DIM;i++){
            for(int j=0;j<DIM;j++){
                full_grid[idx(i,j,DIM)] = rand() % 2;
            }
        }
        long long initial_live = 0;
        for(size_t k=0;k<N;k++) initial_live += full_grid[k];
        printf("Initial live cells: %lld\n", initial_live);
    }

    // prepare counts/displs in elements for MPI_Scatterv (int units)
    int *sendcounts = NULL;
    int *displs = NULL;
    if(rank == 0){
        sendcounts = malloc(size * sizeof(int));
        displs = malloc(size * sizeof(int));
        for(int r=0;r<size;r++){
            sendcounts[r] = rows_counts[r] * row_len;
            displs[r] = rows_displs[r] * row_len;
        }
    }
    // receive into local rows 1..local_rows (skip halo row 0)
    MPI_Scatterv(full_grid, sendcounts, displs, MPI_INT,
                 &local[idx(1,0,row_len)], local_rows * row_len, MPI_INT,
                 0, MPI_COMM_WORLD);

    if(rank == 0){
        free(full_grid);
        free(sendcounts);
        free(displs);
        free(rows_counts);
        free(rows_displs);
    }

    // set halos initial values to 0 (will be overwritten by exchange)
    for(int j=0;j<row_len;j++){
        local[idx(0,j,row_len)] = 0;
        local[idx(local_rows+1, j, row_len)] = 0;
    }

    // prepare neighbors (periodic in vertical direction across processes)
    int prev = (rank - 1 + size) % size;
    int next = (rank + 1) % size;

    MPI_Barrier(MPI_COMM_WORLD);
    double t_start_total = MPI_Wtime();
    double t_loop_start = MPI_Wtime();

    // main loop
    for(int gen=0; gen<GENS; gen++){
        // exchange halos using Sendrecv: send top actual row (1) to prev's bottom halo,
        // and receive prev's last actual row into our top halo (row 0).
        MPI_Sendrecv(&local[idx(1,0,row_len)], row_len, MPI_INT, prev, 0,
                     &local[idx(local_rows+1,0,row_len)], row_len, MPI_INT, next, 0,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);

        // receive bottom neighbor into bottom halo and send our bottom actual row to next neighbor:
        MPI_Sendrecv(&local[idx(local_rows,0,row_len)], row_len, MPI_INT, next, 1,
                     &local[idx(0,0,row_len)], row_len, MPI_INT, prev, 1,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);

        // Now compute next gen for local rows 1..local_rows
        for(int i_local = 1; i_local <= local_rows; i_local++){
            for(int j=0;j<row_len;j++){
                int neighbors = getNeighborsLocal(local, i_local, j, row_len, local_rows);
                int cur = local[idx(i_local,j,row_len)];
                int nextv = cur;
                if(cur){
                    if(neighbors < 2) nextv = 0;
                    else if(neighbors == 2 || neighbors == 3) nextv = 1;
                    else if(neighbors >= 4) nextv = 0;
                } else {
                    if(neighbors == 3) nextv = 1;
                    else nextv = 0;
                }
                local_new[idx(i_local,j,row_len)] = nextv;
            }
        }
        // swap
        int *tmp = local; local = local_new; local_new = tmp;

        // optional: could compute mid-run checks for small gens
    }

    double t_loop_end = MPI_Wtime();
    double t_end_total = MPI_Wtime();

    // reduce final counts
    long long local_sum = 0;
    for(int i_local = 1; i_local <= local_rows; i_local++){
        for(int j=0;j<row_len;j++) local_sum += local[idx(i_local,j,row_len)];
    }
    long long global_sum = 0;
    MPI_Reduce(&local_sum, &global_sum, 1, MPI_LONG_LONG, MPI_SUM, 0, MPI_COMM_WORLD);

    if(rank == 0){
        printf("Final live cells (reduced): %lld\n", global_sum);
        printf("Total wall-clock time (all): %.6f s\n", t_end_total - t_start_total);
        printf("Loop-only time: %.6f s\n", t_loop_end - t_loop_start);
    }

    free(local);
    free(local_new);
    MPI_Finalize();
    return 0;
}
