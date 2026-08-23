#ifndef TEST_INTEROP_CXX_CXX_IMPL_OVERLOADS_H
#define TEST_INTEROP_CXX_CXX_IMPL_OVERLOADS_H

struct Point {
  int x;
  int y;
};

// Same-arity overloads, told apart by parameter type

int overloadedByType(int x);
double overloadedByType(double x);
int overloadedByType(int *p);
int overloadedByType(Point p);

// Overloads told apart by arity and by parameter type

int overloadedByArityAndType(int x);
double overloadedByArityAndType(double x);
int overloadedByArityAndType(int x, int y);
int overloadedByArityAndType(Point p, int z);

// Overloads on enum types

enum EnumFoo { FooA, FooB };
enum class EnumBar { BarA, BarB };

int overloadedByEnumType(int x);
int overloadedByEnumType(unsigned x);
int overloadedByEnumType(EnumFoo x);
int overloadedByEnumType(EnumBar x);

// Both overloads implemented under Swift names, via `@cxx(...)`

int renamedOverload(int x);
double renamedOverload(double x);

// No overload has the implementation's parameter types

int noMatchingOverload(int x);
double noMatchingOverload(double x);

// These two overloads import with the same Swift signature (`const int &`
// imports by value), but their implementation spellings differ: a const
// reference is implemented as a pointer.

int constRefOverload(int x);
int constRefOverload(const int &x);

// Duplicate implementations of one overload

int dupOverload(int x);
double dupOverload(double x);

// The result type is not part of overload identity

int resultMismatchOverload(int x);
double resultMismatchOverload(double x);

// Selection is by parameter types only; whether the selected overload can be
// implemented is checked afterwards

int partiallyInlineOverload(int x);
inline double partiallyInlineOverload(double x) { return x; }

#endif // !TEST_INTEROP_CXX_CXX_IMPL_OVERLOADS_H
