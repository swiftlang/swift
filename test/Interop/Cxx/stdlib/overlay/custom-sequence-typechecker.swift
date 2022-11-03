// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import CustomSequence

func checkIntSequence<S>(_ seq: S) where S: Sequence, S.Element == Int32 {
  let contains = seq.contains(where: { $0 == 3 })
  print(contains)

  for item in seq {
    print(item)
  }
}

// === SimpleSequence ===
// Conformance to UnsafeCxxInputIterator is synthesized.
// Conformance to CxxSequence is synthesized.
checkIntSequence(SimpleSequence())

// === SimpleSequenceWithOutOfLineEqualEqual ===
// Conformance to CxxSequence is synthesized.
checkIntSequence(SimpleSequenceWithOutOfLineEqualEqual())

// === SimpleArrayWrapper ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually UnsafePointers here.
// Conformance to CxxSequence is synthesized.
checkIntSequence(SimpleArrayWrapper())

// === SimpleArrayWrapperNullableIterators ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually optional UnsafePointers here.
// Conformance to CxxSequence is synthesized.
checkIntSequence(SimpleArrayWrapperNullableIterators())

// === SimpleEmptySequence ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually optional UnsafePointers here.
// Conformance to CxxSequence is synthesized.
checkIntSequence(SimpleEmptySequence())
