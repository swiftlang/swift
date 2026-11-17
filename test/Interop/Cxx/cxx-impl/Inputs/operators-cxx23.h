#ifndef TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_CXX23_H
#define TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_CXX23_H

// C++23 allows `operator[]` with any number of parameters.

struct Grid {
  int width;

  // Overloaded by arity.
  int operator[]() const;
  int operator[](int i) const;
  int operator[](int row, int col) const;

  // Overloaded by parameter type.
  double operator[](double row, double col) const;

  int &operator[](int row, int col, int layer);
};

// C++23 also allows a static `operator[]`.

struct StaticGrid {
  static int operator[](int i);
  static int operator[](int row, int col);
};

// Not supported yet

struct StaticCall {
  static int operator()(int x);
};

struct DeducingThis {
  int value;
  bool operator==(this const DeducingThis &self, const DeducingThis &other);
};

// Calls the subscripts from Swift.
int swiftCallsSubscripts(Grid &g);

#endif
