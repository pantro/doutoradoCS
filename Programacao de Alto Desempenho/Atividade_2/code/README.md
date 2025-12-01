Entrega: PPAD - Opción 2
Archivos incluidos:
 - src/main.cpp                (programa principal, pruebas y verificación)
 - src/matmul_orders.hpp       (declaraciones de las 6 órdenes sin blocking)
 - src/matmul_orders.cpp       (implementaciones de las 6 órdenes)
 - src/matmul_blocking.hpp     (declaración de blocked matmul)
 - src/matmul_blocking.cpp     (implementación de blocked matmul)
 - src/strassen.hpp            (declaración Strassen)
 - src/strassen.cpp            (implementación Strassen - simple recursiva)
 - src/blas_wrapper.cpp        (uso de BLAS via cblas_dgemm)
 - Makefile                   (compilación)
 - run_measure.sh             (script para ejecutar casos y medir con perf)

Instrucciones rápidas (Ubuntu):
 1. Instalar dependencias (si no las tienes):
    sudo apt update
    sudo apt install build-essential libblas-dev libopenblas-dev gnuplot -y

 2. Compilar:
    make

 3. Ejecutar (ejemplo):
    ./bin/matmul_exec --mode all --size 128 --reps 3

 4. Medir con perf (ejemplo):
    sudo perf stat -e cycles,instructions,L1-dcache-load-misses,LLC-load-misses -o perf_out.txt ./bin/matmul_exec --mode ijk --size 512

 5. Usar el script de medición para automatizar y obtener CSV:
    chmod +x run_measure.sh
    ./run_measure.sh
