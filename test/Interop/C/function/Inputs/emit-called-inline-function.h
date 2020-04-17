#ifdef __cplusplus
#define INLINE inline
#else
// When compiling as C, make the functions `static inline`. This is the flavor
// of inline functions for which we require the behavior checked by this test.
// Non-static C inline functions don't require Swift to emit LLVM IR for them
// because the idea is that there will we one `.c` file that declares them
// `extern inline`, causing an out-of-line definition to be emitted to the
// corresponding .o file.
#define INLINE static inline
#endif

INLINE int notCalled() {
  return 42;
}

INLINE int calledTransitively() {
  return 42;
}

#ifdef __cplusplus
class C {
 public:
  int memberFunctionCalledTransitively() {
    return 42;
  }
};

inline int calledTransitivelyFromVarInit() {
  return 42;
}

inline int varUsedFromSwift = calledTransitivelyFromVarInit();
#else
// C only allows constant initializers for variables with static storage
// duration, so there's no way to initialize this with the result of a call to
// an inline method. Just provide _some_ definition of `varImportedToSwift` so
// we can import it in the test.
static int varUsedFromSwift = 42;
#endif

INLINE int calledFromSwift() {
#ifdef __cplusplus
  C c;
  return calledTransitively() + c.memberFunctionCalledTransitively();
#else
  return calledTransitively();
#endif
}
