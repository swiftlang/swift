#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_INLINE_H

struct LoadableIntWrapper {
  int value;
};

inline LoadableIntWrapper operator+(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value + rhs.value};
}

inline LoadableIntWrapper operator-(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value - rhs.value};
}

inline LoadableIntWrapper operator*(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value * rhs.value};
}

inline LoadableIntWrapper operator/(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value / rhs.value};
}

inline LoadableIntWrapper operator%(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value % rhs.value};
}

inline LoadableIntWrapper operator^(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value ^ rhs.value};
}

inline LoadableIntWrapper operator&(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value & rhs.value};
}

inline LoadableIntWrapper operator|(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value | rhs.value};
}

inline LoadableIntWrapper operator<<(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value << rhs.value};
}

inline LoadableIntWrapper operator>>(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value >> rhs.value};
}

inline bool operator<(LoadableIntWrapper lhs, LoadableIntWrapper rhs) { return lhs.value < rhs.value; }

inline bool operator>(LoadableIntWrapper lhs, LoadableIntWrapper rhs) { return lhs.value > rhs.value; }

inline bool operator==(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return lhs.value == rhs.value;
}

inline bool operator!=(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return lhs.value != rhs.value;
}

inline bool operator<=(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return lhs.value == rhs.value;
}

inline bool operator>=(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return lhs.value != rhs.value;
}

inline LoadableIntWrapper operator/=(LoadableIntWrapper &lhs, LoadableIntWrapper rhs) {
  lhs.value /= rhs.value;
  return lhs;
}

inline LoadableIntWrapper operator*=(LoadableIntWrapper &lhs, LoadableIntWrapper rhs) {
  lhs.value *= rhs.value;
  return lhs;
}

struct LoadableBoolWrapper {
  bool value;
};

inline LoadableBoolWrapper operator&&(LoadableBoolWrapper lhs, LoadableBoolWrapper rhs) {
  return LoadableBoolWrapper{.value = lhs.value && rhs.value};
}

inline LoadableBoolWrapper operator||(LoadableBoolWrapper lhs, LoadableBoolWrapper rhs) {
  return LoadableBoolWrapper{.value = lhs.value || rhs.value};
}

struct ClassWithOperatorEqualsParamUnnamed {};

inline bool operator==(const ClassWithOperatorEqualsParamUnnamed &,
                       const ClassWithOperatorEqualsParamUnnamed &) {
  return false;
}

struct RValueArithmetic {
  int value;
};

RValueArithmetic operator+(const RValueArithmetic &lhs,
                           RValueArithmetic &&rhs) {
  return {lhs.value + rhs.value};
}

struct LValueAndRValueArithmetic {
  int value;
};

LValueAndRValueArithmetic operator+(const LValueAndRValueArithmetic &lhs,
                                    const LValueAndRValueArithmetic &rhs) {
  return {lhs.value + rhs.value};
}

LValueAndRValueArithmetic operator+(const LValueAndRValueArithmetic &lhs,
                                    LValueAndRValueArithmetic &&rhs) {
  return {lhs.value + rhs.value};
}

// Make sure that we don't crash on templated operators
template<typename T> struct S {};
template<typename T> S<T> operator+(S<T> lhs, S<T> rhs);

#endif
