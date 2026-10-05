#ifndef TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H
#define TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H

namespace Outer {

int add(int a, int b);

void voidNoArgs();
extern int voidNoArgsFlag;

int renamedTarget(int x);

int overloadedByArity(int x);
int overloadedByArity(int x, int y);

int callsSwiftHelper(int x);

inline int inlineFunc(int x) { return x; }

int sameArityOverload(int x);
double sameArityOverload(double x);

int instanceMismatch(int x);

namespace Inner {
int nestedFunc(int x);
int nestedCallsSwiftHelper(int x);
} // namespace Inner

} // namespace Outer

enum PlainEnum { PlainEnumA, PlainEnumB };

#endif // !TEST_INTEROP_CXX_CXX_IMPL_NAMESPACES_H
