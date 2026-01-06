void cfunc1();
void cfunc1(void);
float cfunc2();
float cfunc2(long a, int b);

typedef double (^double_bin_op_block)(double, double);
double_bin_op_block cfunc3(double (^)(double, double));

float cfunc4();

void exit(int);

double pow(double x, double y);
long double powl(long double x, long double y);

void f16ptrfunc(__fp16 *);
#if defined __arm__ || defined __arm64__ || defined __aarch64__
_Float16 f16func(_Float16);
#endif

int puts(const char *);

typedef struct {
  int inode;
} FILE;

FILE *fopen(const char *, const char *);

inline int createSomething(void);

int renamed(int) __asm("_something_else");

void param_pointer(int *p);
void param_const_pointer(const int *p);

void param_void_pointer(void *p);
void param_const_void_pointer(const void *p);

void nonnull_param_pointer(int * _Nonnull p);
void nonnull_param_const_pointer(const int * _Nonnull p);

void nonnull_param_void_pointer(void * _Nonnull p);
void nonnull_param_const_void_pointer(const void * _Nonnull p);

void nested_pointer(const int * const *p);
void nested_pointer_audited(const int * _Nonnull const * _Nullable p);
void nested_pointer_audited2(const int * _Nullable const * _Nonnull p);

void decay_param_array(int p[]);
void decay_param_const_array(const int p[]);

// FIXME: These two should work some day, too.  Right now we don't import
// function types.
void decay_param_func(void g(int));
void decay_param_nested(void g(int p[]));

struct not_importable;

void opaque_pointer_param(struct not_importable *);

int unsupported_parameter_type(int param1, _Complex int param2);
_Complex int unsupported_return_type();

void nullability_note(const char * _Nonnull const * _Nonnull parameter);
