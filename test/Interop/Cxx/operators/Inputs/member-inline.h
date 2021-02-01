#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H

struct LoadableIntWrapper {
  int value;
  LoadableIntWrapper operator-(LoadableIntWrapper rhs) {
    return LoadableIntWrapper{.value = value - rhs.value};
  }
};

struct HasDeletedOperator {
  void operator!=(HasDeletedOperator) const = delete;
};

struct AddressOnlyIntWrapper {
  int value;
  AddressOnlyIntWrapper(int value) : value(value) {}
  AddressOnlyIntWrapper(AddressOnlyIntWrapper const &other)
      : value(other.value) {}
  AddressOnlyIntWrapper operator-(AddressOnlyIntWrapper rhs) {
    return AddressOnlyIntWrapper(value - rhs.value);
  }
};

#endif
