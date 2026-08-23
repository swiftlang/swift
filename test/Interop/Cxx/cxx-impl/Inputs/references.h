#ifndef TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H
#define TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H

// Mutable references, implemented by `UnsafeMutablePointer` parameters.

int addOne(int &x);
void swapRefs(int &a, int &b);

// The bodies of these two observe aliasing: C++ callers may legally pass
// aliasing references, and pointer accesses see every write.

int observe(int &a, int &b);
extern int referencesGlobal;
int observeGlobal(int &x);

// Const references, implemented by `UnsafePointer` parameters.

int readConstRef(const int &x);

// Reference returns, implemented by returning a non-optional pointer.

int &mutableRefReturn();
const int &constRefReturn();
int *_Nonnull &refToPtrReturn();

// A reference to a pointer.

void reseatPtr(int *_Nullable &p);

// Reference-ness participates in overload identity.

void refOverload(int &x);
void refOverload(const int &x);

// A mutable reference and a nonnull pointer overload share one
// implementation spelling; the pair cannot be implemented.

// expected-note@+1{{found this candidate}}
void ambiguousRefOverload(int &x);
// expected-note@+1{{found this candidate}}
void ambiguousRefOverload(int *_Nonnull p);

// Rejected: rvalue references.

void takesRvalueRef(int &&x);
int &&returnsRvalueRef();

// Rejected: wrong parameter spellings.

int mismatchedSpelling(int &x);
int mismatchedConstSpelling(const int &x);

// A method taking a reference.

struct Accumulator {
  int total;
  int addTo(int &target) const;
};

// References to a struct. The referent is an imported record; the
// implementation reads and writes its fields through `pointee`.

void bumpTotal(Accumulator &acc);
int readTotal(const Accumulator &acc);
Accumulator &identityRef(Accumulator &acc);

// A const reference to a pointer, on an operator. The importer does not mark
// an operator's reference parameters addressable, so their lowering reaches
// the same const-reference check as an implementation's pointer spelling; an
// imported operator must still receive the pointer's address.

struct PointerHolder {
  int *_Nonnull p;
};
bool operator==(const PointerHolder &h, int *_Nonnull const &p);
bool holderMatches(const PointerHolder &h, int *_Nonnull p);

#endif // !TEST_INTEROP_CXX_CXX_IMPL_REFERENCES_H
