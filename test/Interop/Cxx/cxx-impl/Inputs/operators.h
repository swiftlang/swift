#ifndef TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_H
#define TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_H

// A member operator imports as an unavailable `__operatorX` method plus a
// synthesized Swift operator function that calls it; a free operator imports
// as a Swift operator function. An implementation is matched by the C++ name.

struct Vector {
  int x;

  bool operator==(const Vector &other) const;
  bool operator<(const Vector &other) const;

  // An overload set: the parameter types select the overload.
  Vector operator+(const Vector &other) const;
  Vector operator+(int k) const;

  // Unary and binary minus differ in arity.
  Vector operator-() const;
  Vector operator-(const Vector &other) const;

  // The importer drops the reference result of a compound assignment; the
  // implementation still returns it, as a pointer.
  Vector &operator+=(const Vector &other);

  int operator[](int i) const;
  int operator()(int i) const;

  // Prefix and postfix increment; the postfix form imports unavailable.
  Vector &operator++();
  Vector operator++(int);
};

bool operator!=(const Vector &a, const Vector &b);
Vector operator*(const Vector &a, int k);

// A free operator in a namespace imports at the top level.
namespace Outer {
struct Point {
  int v;
};
bool operator==(const Point &a, const Point &b);
} // namespace Outer

// Foreign reference type.

struct Handle;
void retainHandle(Handle *_Nonnull);
void releaseHandle(Handle *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainHandle")))
__attribute__((swift_attr("release:releaseHandle"))) Handle {
  int value;

  bool operator==(const Handle &other) const;
  // Returns its referent unretained.
  Handle &operator+=(int k);
};

bool operator<(const Handle &a, const Handle &b);

// Rejections

struct Defined {
  int x;
  bool operator==(const Defined &other) const { return x == other.x; }
  inline bool operator<(const Defined &other) const;
};

struct Rejections {
  bool operator==(const Rejections &other) const;
  Rejections &operator+=(int k);
  // Not imported into Swift.
  Rejections &operator=(const Rejections &other);
};

struct Duplicate {
  int x;
};
bool operator!=(const Duplicate &a, const Duplicate &b);

// Entry point of the execution test's Swift-side checks.
int swiftCallsOperators(const Vector &a, const Vector &b);

#endif
