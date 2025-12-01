README - Experimento Game of Life (MPI)
======================================
1) Compilar:
   make

   Isto gera:
     - serial_gol
     - mpi_gol

2) Testes rápidos (modo debug com dimensão pequena):
   ./serial_gol 256 50
   mpirun -np 4 ./mpi_gol 256 50

3) Experimento final (obrigatório, conforme enunciado):
   Dimensão: 2048x2048  
   Gerações: 2000  
   Processos a avaliar: 1, 2, 4, 8 (no mínimo)  
   Exemplo (em cluster): mpirun -np 8 ./mpi_gol 2048 2000

4) Registro automático:
   Usa o script run_experiments.sh, que gera um arquivo CSV com as colunas:
   np, run, total_time, loop_time, final_live

   Não se esqueça de dar permissões:
     + $ chmod +x run_experiments.sh
     + $ ./run_experiments.sh

5) Métricas a serem reportadas no relatório:
   - Tempo total (wall-clock)
   - Tempo do loop (apenas a região que computa as gerações sucessivas)
   - Speedup: S(p) = T_serial / T_p  (idealmente usar o loop_time)
   - Eficiência: E(p) = S(p) / p
   - Tabela com tempos e contagens finais
   - Mostrar ao menos 1, 2, 4, 8 processos (adicionar mais se houver recursos)

6) Verificações de correção:
   - O enunciado fornece contagens esperadas (com SRAND_VALUE=1985 e DIM=2048):
       Initial: 2096241  
       Generation 1: 1146561  
       Generation 2: 1063629  
       Generation 3: 1052114  
       Generation 4: 1000392  
       ...  
       Generation 2000: 146951  

   - Verifique que as versões serial e MPI produzam o mesmo valor de “Final live cells”.

