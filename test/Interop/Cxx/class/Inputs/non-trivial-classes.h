#ifndef __NON_TRIVIAL_CLASSES__
#define __NON_TRIVIAL_CLASSES__

struct NonTrivial {
  NonTrivial();
  ~NonTrivial();
  NonTrivial(const NonTrivial &);
  int x;
};

void cfunc2(void (*)(NonTrivial));

#endif // __NON_TRIVIAL_CLASSES__
