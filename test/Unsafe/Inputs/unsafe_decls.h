void unsafe_c_function(void) __attribute__((swift_attr("unsafe")));

struct __attribute__((swift_attr("unsafe"))) UnsafeType {
  int field;
};

void print_ints(int *ptr, int count);
