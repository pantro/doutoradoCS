/* gen_data.c
   Uso: ./gen_data N K seed dados.csv centroids.csv
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc,char **argv){
  if(argc<6){ fprintf(stderr,"Usage: %s N K seed dados.csv centroids_in.csv\n",argv[0]); return 1; }
  long N = atol(argv[1]);
  int K = atoi(argv[2]);
  unsigned seed = (unsigned)atoi(argv[3]);
  const char *datafile = argv[4];
  const char *centfile = argv[5];
  srand(seed);
  FILE *fd = fopen(datafile,"w");
  for(long i=0;i<N;i++){
    double v = ((double)rand()/RAND_MAX)*1000.0; // intervalo 0..1000
    fprintf(fd,"%.8f\n", v);
  }
  fclose(fd);
  FILE *fc = fopen(centfile,"w");
  for(int k=0;k<K;k++){
    double v = ((double)rand()/RAND_MAX)*1000.0;
    fprintf(fc,"%.8f\n", v);
  }
  fclose(fc);
  return 0;
}
