/*
  C-program to solve the two-dimensional Poisson equation on 
  a unit square using one-dimensional eigenvalue decompositions
  and fast sine transforms

  einar m. ronquist
  ntnu, october 2000
  revised, october 2001
*/

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>

#include <mpi.h>

/* function prototypes */
double *createdoubleArray (int n);
double **createdouble2DArray (int m, int n);
void transpose (double **bt, double **b, int m);
void fst_(double *v, int *n, double *w, int *nn);
void fstinv_(double *v, int *n, double *w, int *nn);

/* global variables <3 */
static int *local_starts;
static MPI_Datatype *types;

int main(int argc, char **argv )
{
  double *diag, **b, **bt, *buf, *z;
  double pi, h, umax;
  int i, j, n, m, nn;

  /* the total number of grid points in each spatial direction is (n+1) */
  /* the total number of degrees-of-freedom in each spatial direction is (n-1) */
  /* this version requires n to be a power of 2 */

 if( argc < 2 ) {
    printf("need a problem size\n");
    return 1;
  }

  n  = atoi(argv[1]);
  m  = n-1;
  nn = 4*n;

  int size, rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  int m_loc = m / size;
  int extra = m % size;

  local_starts = (int *)malloc((size+1) * sizeof(int));
  for (i = 0; i < size+1; i++) {
    int start = i * m_loc;
    start += (i < extra ? i : extra);
    int end = (i + 1) * m_loc;
    end += (i+1 < extra ? i+1 : extra);
    local_starts[i] = start;
  }

  m_loc = local_starts[rank+1] - local_starts[rank];

  types = (MPI_Datatype *)malloc(size * sizeof(MPI_Datatype));
  for (i = 0; i < size; i++) {
    MPI_Type_vector(m_loc, local_starts[i+1] - local_starts[i],
          m, MPI_DOUBLE, types + i);
    MPI_Type_commit(types + i);
  }

  diag = createdoubleArray (m);
  b    = createdouble2DArray (m_loc,m);
  bt   = createdouble2DArray (m_loc,m);
  buf  = createdoubleArray (m_loc*m);
  z    = createdoubleArray (nn);

  h    = 1./(double)n;
  pi   = 4.*atan(1.);

  for (i=0; i < m; i++) {
    diag[i] = 2.*(1.-cos((i+1)*pi/(double)n));
  }
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      b[j][i] = h*h;
    }
  }
  for (j=0; j < m_loc; j++) {
    fst_(b[j], &n, z, &nn);
  }

  transpose (bt,b,m);

  for (i=0; i < m_loc; i++) {
    fstinv_(bt[i], &n, z, &nn);
  }
  
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      bt[j][i] = bt[j][i]/(diag[local_starts[rank]+i]+diag[local_starts[rank]+j]);
    }
  }
  
  for (i=0; i < m_loc; i++) {
    fst_(bt[i], &n, z, &nn);
  }

  transpose (b,bt,m);

  for (j=0; j < m_loc; j++) {
    fstinv_(b[j], &n, z, &nn);
  }

  umax = 0.0;
  for (j=0; j < m_loc; j++) {
    for (i=0; i < m; i++) {
      if (b[j][i] > umax) umax = b[j][i];
    }
  }
  printf (" umax = %e \n",umax);

  MPI_Finalize();

  return 0;
}

void blockTranspose (double **bt, double **b, double *buf, int m, int size, int rank)
{
  // TODO: Do an alltoallv, make a recvbuf to get the packed doubles, 
  //       use MPI_Unpack to get them out. Do a local transpose. 
  //       Profit? Maybe.
  int i, pos=0;
  for (i = 0; i < size; i++) {
    MPI_Pack(*b + *(local_starts+i), 1, *(types + i), buf, sizeof(*b), &pos, MPI_COMM_WORLD);
  }

}

void transpose (double **bt, double **b, int m)
{
  int i, j;
  for (j=0; j < m; j++) {
    for (i=0; i < m; i++) {
      bt[j][i] = b[i][j];
    }
  }
}

double *createdoubleArray (int n)
{
  double *a;
  int i;
  a = (double *)malloc(n*sizeof(double));
  for (i=0; i < n; i++) {
    a[i] = 0.0;
  }
  return (a);
}

double **createdouble2DArray (int n1, int n2)
{
  int i, n;
  double **a;
  a    = (double **)malloc(n1   *sizeof(double *));
  a[0] = (double  *)malloc(n1*n2*sizeof(double));
  for (i=1; i < n1; i++) {
    a[i] = a[i-1] + n2;
  }
  n = n1*n2;
  memset(a[0],0,n*sizeof(double));
  return (a);
}
