#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_FROM_NESTED_STRUCT_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_FROM_NESTED_STRUCT_H

inline int increment(int t) { return t + 1; }

struct IncrementUser {
  struct Incrementor {
    int value;
    Incrementor(int v) { value = increment(v); }
  };
};

inline int callConstructor(int value) {
  return IncrementUser::Incrementor(value).value;
}

inline int get42() { return 42; }

struct HoldMemberThatHolds42 {
  struct Hold42 {
    int m = get42();
  };

  Hold42 holder;
};

struct HoldMemberThatHoldsMemberThatHolds42 {
  HoldMemberThatHolds42 holder;
};

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_FROM_NESTED_STRUCT_H
