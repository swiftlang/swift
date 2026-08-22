#ifndef TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H
#define TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H

namespace Outer {

int add(int a, int b);

int renamedTarget(int x);

int overloadedByArity(int x);
int overloadedByArity(int x, int y);

inline int inlineFunc(int x) { return x; }

int sameArityOverload(int x);
double sameArityOverload(double x);

int instanceMismatch(int x);

namespace Inner {
int nestedFunc(int x);
} // namespace Inner

} // namespace Outer

enum PlainEnum { PlainEnumA, PlainEnumB };

#endif // !TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H
