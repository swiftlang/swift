// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import CustomSequence
import Cxx

// === SimpleSequence ===
// Conformance to UnsafeCxxInputIterator is synthesized.
extension SimpleSequence: CxxSequence {}

func checkSimpleSequence() {
  let seq = SimpleSequence()
  let contains = seq.contains(where: { $0 == 3 })
  print(contains)

  for item in seq {
    print(item)
  }
}

// === SimpleSequenceWithOutOfLineEqualEqual ===
extension SimpleSequenceWithOutOfLineEqualEqual : CxxSequence {}

// === SimpleArrayWrapper ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually UnsafePointers here.
extension SimpleArrayWrapper: CxxSequence {}

// === SimpleArrayWrapperNullableIterators ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually optional UnsafePointers here.
extension SimpleArrayWrapperNullableIterators: CxxSequence {}

// === SimpleEmptySequence ===
// No UnsafeCxxInputIterator conformance required, since the iterators are actually optional UnsafePointers here.
extension SimpleEmptySequence: CxxSequence {}
