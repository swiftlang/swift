#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_ANY_REFERENCE_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_ANY_REFERENCE_H

#include <swift/bridging>

#pragma clang assume_nonnull begin

struct RefCountedType {
  int value;
  mutable int refCount = 1;

  SWIFT_RETURNS_RETAINED
  RefCountedType(int value) : value(value) {}

  RefCountedType(const RefCountedType &) = delete;

  virtual ~RefCountedType() {}

  // Returns `this` without transferring ownership: the same object, again.
  SWIFT_RETURNS_UNRETAINED
  RefCountedType *getSelf() { return this; }
} SWIFT_SHARED_REFERENCE(retainRefCounted, releaseRefCounted);

inline void retainRefCounted(RefCountedType *obj) {
  obj->refCount++;
}

inline void releaseRefCounted(RefCountedType *obj) {
  if (--obj->refCount == 0)
    delete obj;
}

struct DerivedRefCountedType : RefCountedType {
  SWIFT_RETURNS_RETAINED
  DerivedRefCountedType(int value) : RefCountedType(value) {}

  // Upcast in C++; the base is at offset zero, so the pointer is unchanged.
  SWIFT_RETURNS_UNRETAINED
  RefCountedType *asBase() { return this; }
};

// An immortal foreign reference type.
struct ImmortalRefType {
  int value;
} SWIFT_IMMORTAL_REFERENCE;

struct ValueType {
  int value;
  ValueType(int value) : value(value) {}
};

#pragma clang assume_nonnull end

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_ANY_REFERENCE_H
