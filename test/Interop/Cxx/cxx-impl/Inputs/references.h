#ifndef TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H
#define TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H

// Mutable references, implemented by `inout` parameters.

int addOne(int &x);
void swapRefs(int &a, int &b);

// Const references, implemented by plain by-value parameters.

int readConstRef(const int &x);
extern int referencesGlobal;

// Reference returns, implemented by returning a non-optional pointer.

int &mutableRefReturn();
const int &constRefReturn();
int *_Nonnull &refToPtrReturn();

// A reference to a pointer.

void reseatPtr(int *_Nullable &p);

// Reference-ness participates in overload identity.

void refOverload(int &x);
void refOverload(const int &x);
void refOverload(int *_Nonnull p);

// A method taking a reference.

struct Accumulator {
  int total;
  int addTo(int &target) const;
};

// References to a struct.

void bumpTotal(Accumulator &acc);
int readTotal(const Accumulator &acc);

// Rejected: implementations with reference parameters that are not marked
// `@unsafe`.

// expected-note@+1{{'x' declared as a C++ reference here}}
int missingUnsafeParam(int &x);
// expected-note@+1{{'x' declared as a C++ reference here}}
int missingUnsafeConstParam(const int &x);
// expected-note@+2{{'b' declared as a C++ reference here}}
// expected-note@+1{{'c' declared as a C++ reference here}}
void missingUnsafeMixed(int a, int &b, const int &c);
// expected-note@+1{{parameter declared as a C++ reference here}}
int missingUnsafeUnnamed(int &);

// Rejected: rvalue references.

void takesRvalueRef(int &&x);
int &&returnsRvalueRef();

// Rejected: spelling a reference parameter as a pointer.

int mismatchedSpelling(int &x);
int mismatchedConstSpelling(const int &x);

#endif // !TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H
