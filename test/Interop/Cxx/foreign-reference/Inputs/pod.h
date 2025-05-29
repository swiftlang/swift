#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_POD_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_POD_H

#include <stdlib.h>
#if defined(_WIN32)
inline void *operator new(size_t, void *p) { return p; }
#else
#include <new>
#endif

#include "visibility.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Empty {
  int test() const { return 42; }
  int testMutable() { return 42; }

  static Empty *create() { return new (malloc(sizeof(Empty))) Empty(); }
};

void takesConstRefEmpty(const Empty &e) {}
void takesConstRefEmptyDefaulted(const Empty &e = {}) {}
void mutateIt(Empty &) {}
Empty passThroughByValue(Empty x) { return x; }

struct __attribute__((swift_attr("@actor")))
__attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) MultipleAttrs {
  int test() const { return 42; }
  int testMutable() { return 42; }

  static MultipleAttrs *create() {
    return new (malloc(sizeof(MultipleAttrs))) MultipleAttrs();
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) IntPair {
  int a = 1;
  int b = 2;

  int test() const { return b - a; }
  int testMutable() { return b - a; }

  IntPair &instancePassThroughByRef(IntPair& ref) { return ref; }
  static IntPair &staticPassThroughByRef(IntPair& ref) { return ref; }
  static IntPair *create() { return new (malloc(sizeof(IntPair))) IntPair(); }
};

void mutateIt(IntPair &x) {
  x.a = 2;
  x.b = 4;
}
IntPair passThroughByValue(IntPair x) { return x; }
IntPair &passThroughByRef(IntPair &x) { return x; }

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) RefHoldingPair {
  // REVIEW-NOTE: I added support for this but then removed it, as this sort of
  // indicates incorrect usage of a "reference type" and has weird semantics.
  IntPair pair;
  int otherValue = 3;

  int test() const { return otherValue - pair.test(); }
  int testMutable() { return otherValue - pair.test(); }

  static RefHoldingPair *create() {
    return new (malloc(sizeof(RefHoldingPair))) RefHoldingPair();
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) RefHoldingPairRef {
  IntPair &pair;
  int otherValue;
  RefHoldingPairRef(IntPair &pair) : pair(pair), otherValue(42) {}

  int test() const { return otherValue - pair.test(); }
  int testMutable() { return otherValue - pair.test(); }

  static RefHoldingPairRef *create() {
    IntPair *pair = new (malloc(sizeof(IntPair))) IntPair();
    return new (malloc(sizeof(RefHoldingPairRef))) RefHoldingPairRef(*pair);
  }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) RefHoldingPairPtr {
  IntPair *pair;
  int otherValue = 42;

  int test() const { return otherValue - pair->test(); }
  int testMutable() { return otherValue - pair->test(); }

  static RefHoldingPairPtr *create() {
    RefHoldingPairPtr *out =
        new (malloc(sizeof(RefHoldingPairPtr))) RefHoldingPairPtr();
    out->pair = new (malloc(sizeof(IntPair))) IntPair();
    return out;
  }
};

struct ValueHoldingPair {
  IntPair pair;
  int otherValue = 3;

  int test() const { return otherValue - pair.test(); }
  int testMutable() { return otherValue - pair.test(); }

  static ValueHoldingPair *create()
      __attribute__((swift_attr("import_unsafe"))) {
    return new (malloc(sizeof(ValueHoldingPair))) ValueHoldingPair();
  }
};

struct ValueHoldingPairRef {
  IntPair &pair = *IntPair::create();

  int sub(const IntPair &other) const {
    return pair.test() - other.test();
  };
  const IntPair &max(const IntPair &other) const {
    return pair.test() > other.test() ? pair : other;
  };
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) BigType {
  int a = 1;
  int b = 2;
  char buffer[32];

  int test() const { return b - a; }
  int testMutable() { return b - a; }

  static BigType *create() { return new (malloc(sizeof(BigType))) BigType(); }
};

void mutateIt(BigType &x) {
  x.a = 2;
  x.b = 4;
}
BigType passThroughByValue(BigType x) { return x; }

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) BaseRef {
  int a = 1;
  int b = 2;

  int test() const { return b - a; }
  int test() { return b - a; }

  static BaseRef *create() { return new (malloc(sizeof(BaseRef))) BaseRef(); }
};

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) DerivedRef : BaseRef {
  int c = 1;

  int testDerived() const { return test() + c; }
  int testDerived() { return test() + c; }

  static DerivedRef *create() { return new (malloc(sizeof(DerivedRef))) DerivedRef(); }
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_POD_H
