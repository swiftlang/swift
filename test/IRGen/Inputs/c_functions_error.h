static inline void produce_error_in_clang_irgen() {
  typedef double __m128d __attribute__((__vector_size__(16)));

  __m128d a, b;
  __builtin_ia32_addsubps(b, a);
}
