// RUN: %target-parse-verify-swift

import StdlibUnittest

//
// Check that CollectionType._prext_SubSlice is constrained to CollectionType.
//

// expected-error@+2 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'SequenceType'}}
// expected-error@+1 {{type 'CollectionWithBadSubSequence' does not conform to protocol 'CollectionType'}}
struct CollectionWithBadSubSequence : CollectionType {
  var startIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  var endIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  subscript(i: MinimalForwardIndex) -> OpaqueValue<Int> {
    fatalError("unreachable")
  }

  // expected-note@+1 {{possibly intended match '_prext_SubSequence' does not conform to 'Indexable'}}
  typealias _prext_SubSequence = OpaqueValue<Int8>
}

func useCollectionTypeSubSequenceIndex<
  C : CollectionType
  where
  C._prext_SubSequence.Index : BidirectionalIndexType
>(c: C) {}

func useCollectionTypeSubSequenceGeneratorElement<
  C : CollectionType
  where
  C._prext_SubSequence.Generator.Element == C.Generator.Element
>(c: C) {}

func sortResultIgnored<
  S : SequenceType, MC : MutableCollectionType
  where
  S.Generator.Element : Comparable,
  MC.Generator.Element : Comparable
>(
  var sequence: S, // expected-warning {{parameter 'sequence' was never mutated; consider changing to 'let' constant}}
  var mutableCollection: MC, // expected-warning {{parameter 'mutableCollection' was never mutated; consider changing to 'let' constant}}
  var array: [Int] // expected-warning {{parameter 'array' was never mutated; consider changing to 'let' constant}}
) {

  sequence.sort() // expected-warning {{result of call to 'sort()' is unused}}
  sequence.sort { $0 < $1 } // expected-warning {{result of call to 'sort' is unused}}

  mutableCollection.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}}
  mutableCollection.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}}

  array.sort() // expected-warning {{result of call to non-mutating function 'sort()' is unused; use 'sortInPlace()' to mutate in-place}}
  array.sort { $0 < $1 } // expected-warning {{result of call to non-mutating function 'sort' is unused; use 'sortInPlace' to mutate in-place}}
}

