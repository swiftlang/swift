#ifndef TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_H
#define TEST_INTEROP_CXX_CXX_IMPL_OPERATORS_H

struct Vector {
  int x;

  bool operator==(const Vector &other) const;
  bool operator<(const Vector &other) const;

  // Overloaded by parameter type.
  Vector operator+(const Vector &other) const;
  Vector operator+(int k) const;

  // Overloaded by arity.
  Vector operator-() const;
  Vector operator-(const Vector &other) const;

  // The importer drops the reference result.
  Vector &operator+=(const Vector &other);

  int operator[](int i) const;
  int operator()(int i) const;

  // Prefix and postfix; postfix imports unavailable.
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

// Foreign reference type

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

// Implemented by functions named with raw identifiers

struct RawIdentifier {
  int x;

  bool operator==(const RawIdentifier &other) const;
  int operator[](int i) const;
  int operator()(int i) const;
};

bool operator!=(const RawIdentifier &a, const RawIdentifier &b);

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
  // Return rvalue references.
  Rejections &&operator-=(int k);
  Rejections &&operator+(int k) const;
};

struct Duplicate {
  int x;
};
bool operator!=(const Duplicate &a, const Duplicate &b);

// Not supported yet

struct ConversionTarget {
  int v;
};

struct Convertible {
  int value;
  operator bool() const;
  operator int() const;
  operator ConversionTarget() const;
};

// Calls the operators from Swift.
int swiftCallsOperators(const Vector &a, const Vector &b);

#endif
