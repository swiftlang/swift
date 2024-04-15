#ifndef __CLOSURE__
#define __CLOSURE__

struct NonTrivial {
  NonTrivial() { p = new int(123); }
  ~NonTrivial() { delete p; }
  int *p;
};

void cfunc2(void (*fp)(NonTrivial)) {
  NonTrivial t;
  (*fp)(t);
}

#endif // __CLOSURE__
