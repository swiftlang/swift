struct extern_struct {
  int field : 1;
};

typedef enum ext_enum {
  p, q, r
} extern_enum;

static inline int extern_inline_function(int a, int b) {
  return a + b;
}
