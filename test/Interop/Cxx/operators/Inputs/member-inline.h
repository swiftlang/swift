#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H

struct LoadableIntWrapper {
  int value;
  LoadableIntWrapper operator-(LoadableIntWrapper rhs) {
    return LoadableIntWrapper{.value = value - rhs.value};
  }

  int operator()() {
    return value;
  }
  int operator()(int x) {
    return value + x;
  }
  int operator()(int x, int y) {
    return value + x * y;
  }
};

struct AddressOnlyIntWrapper {
  int value;

  AddressOnlyIntWrapper(int value) : value(value) {}
  AddressOnlyIntWrapper(const AddressOnlyIntWrapper &other) : value(other.value) {}

  int operator()() {
    return value;
  }
  int operator()(int x) {
    return value + x;
  }
  int operator()(int x, int y) {
    return value + x * y;
  }
};

struct HasDeletedOperator {
  void operator!=(HasDeletedOperator) const = delete;
};

#endif
