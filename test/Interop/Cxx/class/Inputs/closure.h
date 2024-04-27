#ifndef __CLOSURE__
#define __CLOSURE__

struct NonTrivial {
  NonTrivial() { p = new int(123); }
  ~NonTrivial() { delete p; }
  NonTrivial(const NonTrivial &other);
  int *p;
};

void cfunc2(void (*fp)(NonTrivial)) {
  (*fp)(NonTrivial());
}

#if __OBJC__
struct ARCStrong {
  id a;
};

void cfuncARCStrong(void (*_Nonnull)(ARCStrong));
#endif

#endif // __CLOSURE__
