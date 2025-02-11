#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMBER_VARIABLES_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMBER_VARIABLES_H

class MyClass {
public:
  const int const_member = 23;
};

struct Empty {
  using type = int;
  int getNum() const { return 42; }
};

struct HasZeroSizedField {
  int a;
  [[no_unique_address]] Empty b;
  short c;
  [[no_unique_address]] Empty d;
  int* e;
  [[no_unique_address]] Empty f;

  int get_a() const { return a; }
  short get_c() const { return c; }
  void set_c(short c) { this->c = c; }
};

inline int takesZeroSizedInCpp(HasZeroSizedField x) {
  return x.a;
}

#endif
