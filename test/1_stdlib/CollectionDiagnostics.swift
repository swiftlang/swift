// RUN: %target-parse-verify-swift

import StdlibUnittest

//
// Check that CollectionType._prext_SubSlice is constrained to CollectionType.
//

// expected-error@+2 {{type 'CollectionWithBadSubSlice' does not conform to protocol '_CollectionDefaultsType'}}
// expected-error@+1 {{type 'CollectionWithBadSubSlice' does not conform to protocol 'CollectionType'}}
struct CollectionWithBadSubSlice : CollectionType {
  var startIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  var endIndex: MinimalForwardIndex {
    fatalError("unreachable")
  }

  subscript(i: MinimalForwardIndex) -> OpaqueValue<Int> {
    fatalError("unreachable")
  }

  // expected-note@+1 {{possibly intended match '_prext_SubSlice' does not conform to '_CollectionDefaultsType'}}
  typealias _prext_SubSlice = OpaqueValue<Int8>
}

func useCollectionTypeSubSliceIndex<
  C : CollectionType
  where
  C._prext_SubSlice.Index : BidirectionalIndexType
>(c: C) {}

func useCollectionTypeSubSliceGeneratorElement<
  C : CollectionType
  where
  C._prext_SubSlice.Generator.Element == C.Generator.Element
>(c: C) {}

