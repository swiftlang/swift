#ifndef TEST_INTEROP_CXX_CXX_IMPL_MATCHING_H
#define TEST_INTEROP_CXX_CXX_IMPL_MATCHING_H

// Existing declarations

int existingDeclaration1(int x);
int existingDeclaration2(int x);
int existingDeclaration3(int x);

// Duplicates

int dupFunc(int x);

// Runtime clobbers

extern "C" int swift_retain(int x);
int swift_release(int x);
int funcWithSwiftRetainAsmLabel(int x) __asm__("swift_retain");

// Inline

inline int inlineDefinition(int x) { return x; }
inline int inlineDeclaration(int x);
constexpr int constexprFunc(int x);

// Internal linkage

static int staticFunc(int x);

// Type mismatch

int typeMismatchParam(int x);
int typeMismatchReturn(int x);
int typeMismatchParamExplicitName(int x);
int typeMismatchReturnExplicitName(int x);
int typeMismatchRenamed(int x);
int typeMatchRenamed(float x);

// Templates

template <typename T>
T templateFunc(T x);

// Variadic args

int variadicFunc(int x, ...);

// Overloads

int sameArityOverload(int x);
double sameArityOverload(double x);

// C++ references

int takesConstRef(const int &x);
void takesMutableRef(int &x);
int &returnsMutableRef();

#endif // !TEST_INTEROP_CXX_CXX_IMPL_MATCHING_H
