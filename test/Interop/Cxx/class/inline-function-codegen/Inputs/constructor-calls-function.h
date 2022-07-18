#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H

inline int increment(int t) { return t + 1; }

struct Incrementor {
  int incrementee;
  Incrementor(int value) : incrementee(increment(value)) {}
};

inline int callConstructor(int value) { return Incrementor(value).incrementee; }

inline int get42() { return 42; }

template <typename T>
T passThroughArgT(T t) {
  return t;
}

struct Hold42 {
  int m = get42();
};

struct Hold23 {
  int m = passThroughArgT<int>(23);
};

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H
