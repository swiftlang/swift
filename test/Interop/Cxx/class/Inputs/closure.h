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

struct ARCWeak {
#if __OBJC__
  __weak _Nullable id m;
#endif
};

void cfuncReturnNonTrivial(NonTrivial (^_Nonnull)()) noexcept;
void cfuncReturnNonTrivial2(NonTrivial (*_Nonnull)()) noexcept;

void (*_Nonnull getFnPtr2() noexcept)(ARCWeak) noexcept;

#endif // __CLOSURE__
