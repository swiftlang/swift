void cfunc1(void);
float cfunc2(long a, int b);

typedef double (^double_bin_op_block)(double, double);
double_bin_op_block cfunc3(double (^)(double, double));

void exit(int);

double pow(double, double);

int puts(const char *);

typedef struct {
  int inode;
} FILE;

FILE *fopen(const char *, const char *);
