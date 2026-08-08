#ifndef FUNCTIONS_H
#define FUNCTIONS_H

int existingDeclaration1(int x);
int existingDeclaration2(int x);
int existingDeclaration3(int x);

int dupFunc(int x);

extern "C" int swift_retain(int x);
int swift_release(int x);
int funcWithSwiftRetainAsmLabel(int x) __asm__("swift_retain");

inline int inlineDefinition(int x) { return x; }
inline int inlineDeclaration(int x);
constexpr int constexprFunc(int x);

static int staticFunc(int x);

int typeMismatchParam(int x);
int typeMismatchReturn(int x);
int typeMismatchParamExplicitName(int x);
int typeMismatchReturnExplicitName(int x);
int typeMismatchRenamed(int x);
int typeMatchRenamed(float x);

template <typename T>
T templateFunc(T x);

int variadicFunc(int x, ...);

int sameArityOverload(int x);
double sameArityOverload(double x);

int refParam(int &x);
int constRefParam(const int &x);
int &refReturn();

#endif // !FUNCTIONS_H
