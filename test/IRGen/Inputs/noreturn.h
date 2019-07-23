int scalarNoReturn() __attribute__((noreturn));

void voidNoReturn() __attribute__((noreturn));

typedef struct {
  long long a;
  long long b;
  long long c;
  long long d;
} Large;

Large largeStructNoReturn() __attribute__((noreturn));

typedef struct {
  long a;
  long b;
} Small;

Small smallStructNoReturn() __attribute__((noreturn));
