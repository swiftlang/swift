#ifndef TEST_INTEROP_CXX_CXX_IMPL_FUNCTIONS_H
#define TEST_INTEROP_CXX_CXX_IMPL_FUNCTIONS_H

struct TrivialStruct {
  int x;
  int y;
};

class NonTrivialClass {
public:
  NonTrivialClass() {}
  NonTrivialClass(const NonTrivialClass &other) : value(other.value) {}
  ~NonTrivialClass() {}
  int value;
};

// Simple functions

int foo(int x);
int bar(int x);

// A C++ function name that is a Swift keyword.
int defer(int x);

// Primitives

void takesPrimitives(int i, long l, char c, float f, double d, bool b);
int returnsInt();

// Pointers

void takesPtrToInt(int *p);
void takesNullablePtrToInt(int *_Nullable p);
void takesNonnullPtrToInt(int *_Nonnull p);

void takesPtrToVoid(void *p);
void takesNullablePtrToVoid(void *_Nullable p);
void takesNonnullPtrToVoid(void *_Nonnull p);

void takesPtrToConstInt(const int *p);
void takesNullablePtrToConstInt(const int *_Nullable p);
void takesNonnullPtrToConstInt(const int *_Nonnull p);

void takesFuncPtr(int (*fn)(int));
void takesNullableFuncPtr(int (*_Nullable fn)(int));
void takesNonnullFuncPtr(int (*_Nonnull fn)(int));

int *returnsPtrToInt();
int *_Nullable returnsNullablePtrToInt();
int *_Nonnull returnsNonnullPtrToInt();

// Trivial struct

void takesTrivialStruct(TrivialStruct s);
TrivialStruct returnsTrivialStruct();

#endif // !TEST_INTEROP_CXX_CXX_IMPL_FUNCTIONS_H
