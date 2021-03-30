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

struct ReadWriteIntArray {
private:
  int values[5] = { 1, 2, 3, 4, 5 };

public:
  const int &operator[](int x) const {
    return values[x];
  }
  int &operator[](int x) {
    return values[x];
  }

  struct NestedIntArray {
  private:
    int values[5] = { 1, 2, 3, 4, 5 };

  public:
    const int &operator[](int x) const {
      return values[x];
    }
  };
};

struct ReadOnlyIntArray {
private:
  int values[5] = { 1, 2, 3, 4, 5 };

public:
  ReadOnlyIntArray(int first) {
    values[0] = first;
  }
  ReadOnlyIntArray(const ReadOnlyIntArray &other) {}

  const int &operator[](int x) const {
    return values[x];
  }
};

struct WriteOnlyIntArray {
private:
  int values[5] = { 1, 2, 3, 4, 5 };

public:
  int &operator[](int x) {
    return values[x];
  }
};

struct DifferentTypesArray {
private:
  int values[3] = { 1, 2, 3 };
  double doubleValues[2] = { 1.5, 2.5 };
  bool boolValues[2] = { true, false };

public:
  const int &operator[](int x) const {
    return values[x];
  }
  int &operator[](int x) {
    return values[x];
  }
  bool &operator[](bool x) {
    return boolValues[x];
  }
  const bool &operator[](bool x) const {
    return boolValues[x];
  }
  const double &operator[](double x) const {
    return doubleValues[x == 0.0];
  }
};

template<class T>
struct TemplatedArray {
  T ts[];

  T &operator[](int i) {
    return ts[i];
  }
  const T &operator[](int i) const {
    return ts[i];
  }
};
typedef TemplatedArray<double> TemplatedDoubleArray;

struct TemplatedSubscriptArray {
  int *ptr;

  template<class T>
  T &operator[](T i) {
    return ptr[i];
  }
  template<class T>
  const T &operator[](T i) const {
    return ptr[i];
  }
};

struct NonReferenceReadIntArray {
private:
  int values[3] = { 1, 2, 3 };

public:
  int operator[](int x) const {
    return values[x];
  }
};

#endif
