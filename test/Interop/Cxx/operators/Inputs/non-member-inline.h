#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_INLINE_H

struct IntBox {
  int value;
};

inline IntBox operator+(IntBox lhs, IntBox rhs) {
  return IntBox{.value = lhs.value + rhs.value};
}

inline IntBox operator-(IntBox lhs, IntBox rhs) {
  return IntBox{.value = lhs.value - rhs.value};
}

inline IntBox operator*(IntBox lhs, IntBox rhs) {
  return IntBox{.value = lhs.value * rhs.value};
}

inline IntBox operator/(IntBox lhs, IntBox rhs) {
  return IntBox{.value = lhs.value / rhs.value};
}

// Make sure that we don't crash on templated operators
template<typename T> struct S {};
template<typename T> S<T> operator+(S<T> lhs, S<T> rhs);

#endif
