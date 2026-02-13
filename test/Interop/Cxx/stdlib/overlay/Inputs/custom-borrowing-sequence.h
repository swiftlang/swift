#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_BORROWING_SEQUENCE_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_BORROWING_SEQUENCE_H

#include "custom-iterator.h"
#include "swift/bridging"

struct SWIFT_NONCOPYABLE NonCop { int number; };

struct SWIFT_NONCOPYABLE SimpleNonCopyableSequence {
  ConstIterator begin() const { return ConstIterator(1); }
  ConstIterator end() const { return ConstIterator(5); }
};

template<typename T>
struct SWIFT_COPYABLE_IF(T) SimpleConditionallyCopyableSequence {
  ConstIterator begin() const { return ConstIterator(1); }
  ConstIterator end() const { return ConstIterator(5); }
};

using SimpleIntSequence = SimpleConditionallyCopyableSequence<int>;
using SimpleNonCopSequence = SimpleConditionallyCopyableSequence<NonCop>;

struct SWIFT_NONCOPYABLE SimpleNonCopArrayWrapper {
private:
  NonCop a[5] = {{10}, {20}, {30}, {40}, {50}};

public:
  const NonCop *begin() const __attribute__((returns_nonnull)) { return &a[0]; }
  const NonCop *end() const __attribute__((returns_nonnull)) { return &a[5]; }
};

struct SWIFT_NONCOPYABLE NonReferenceDereferenceOperatorSequence {
  NonReferenceDereferenceOperator begin() const {
    return NonReferenceDereferenceOperator(1);
  }
  NonReferenceDereferenceOperator end() const {
    return NonReferenceDereferenceOperator(5);
  }
};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_BORROWING_SEQUENCE_H
