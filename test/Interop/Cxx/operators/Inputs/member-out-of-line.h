#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_OUT_OF_LINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_OUT_OF_LINE_H

struct LoadableIntWrapper {
  int value;
  LoadableIntWrapper operator+(LoadableIntWrapper rhs) const;
  int operator()() const;
  int operator()(int x) const;
  int operator()(int x, int y) const;
};

struct AddressOnlyIntWrapper {
  int value;

  AddressOnlyIntWrapper(int value) : value(value) {}
  AddressOnlyIntWrapper(const AddressOnlyIntWrapper &other) : value(other.value) {}

  int operator()() const;
  int operator()(int x) const;
  int operator()(int x, int y) const;
};

struct ReadWriteIntArray {
private:
  int values[5] = { 1, 2, 3, 4, 5 };

public:
  const int &operator[](int x) const;
  int &operator[](int x);
};

#endif
