// RUN: %target-parse-verify-swift

// Positive and negative tests for String index types

// Positive and negative tests for String collection types. Testing the complete
// set of conformance just in case protocol hierarchy changes.
func acceptsCollection<C: Collection>(c: C) {}
func acceptsBidirectionalCollection<C: BidirectionalCollection>(c: C) {}
func acceptsRandomAccessCollection<C: RandomAccessCollection>(c: C) {}

func testStringCollectionTypes(s: String) {
  acceptsCollection(s.utf8)
  acceptsBidirectionalCollection(s.utf8) // expected-error{{argument type 'String.UTF8View' does not conform to expected type 'BidirectionalCollection'}}
  acceptsRandomAccessCollection(s.utf8) // expected-error{{argument type 'String.UTF8View' does not conform to expected type 'RandomAccessCollection'}}

  // UTF16View.Index is random-access with Foundation, bidirectional without
  acceptsCollection(s.utf16)
  acceptsBidirectionalCollection(s.utf16)
  acceptsRandomAccessCollection(s.utf16) // expected-error{{argument type 'String.UTF16View' does not conform to expected type 'RandomAccessCollection'}}

  acceptsCollection(s.unicodeScalars)
  acceptsBidirectionalCollection(s.unicodeScalars)
  acceptsRandomAccessCollection(s.unicodeScalars) // expected-error{{argument type 'String.UnicodeScalarView' does not conform to expected type 'RandomAccessCollection'}}

  acceptsCollection(s.characters)
  acceptsBidirectionalCollection(s.characters)
  acceptsRandomAccessCollection(s.characters) // expected-error{{argument type 'String.CharacterView' does not conform to expected type 'RandomAccessCollection'}}
}

// Positive and negative tests for String index types. Testing the complete
// set of conformance just in case protocol hierarchy changes.
func acceptsCollectionIndex<I: Comparable>(index: I) {}
func acceptsBidirectionalCollectionIndex<I: Comparable>(index: I) {}
func acceptsRandomAccessCollectionIndex<I: Strideable>(index: I) {}

func testStringIndexTypes(s: String) {
  acceptsCollectionIndex(s.utf8.startIndex)
  acceptsBidirectionalCollectionIndex(s.utf8.startIndex)
  acceptsRandomAccessCollectionIndex(s.utf8.startIndex) // expected-error{{argument type 'String.UTF8View.Index' does not conform to expected type 'Strideable'}}

  // UTF16View.Index is random-access with Foundation, bidirectional without
  acceptsCollectionIndex(s.utf16.startIndex)
  acceptsBidirectionalCollectionIndex(s.utf16.startIndex)
  acceptsRandomAccessCollectionIndex(s.utf16.startIndex)  // expected-error{{argument type 'String.UTF16View.Index' does not conform to expected type 'Strideable'}}

  acceptsCollectionIndex(s.unicodeScalars.startIndex)
  acceptsBidirectionalCollectionIndex(s.unicodeScalars.startIndex)
  acceptsRandomAccessCollectionIndex(s.unicodeScalars.startIndex) // expected-error{{argument type 'String.UnicodeScalarView.Index' does not conform to expected type 'Strideable'}}

  acceptsCollectionIndex(s.characters.startIndex)
  acceptsBidirectionalCollectionIndex(s.characters.startIndex)
  acceptsRandomAccessCollectionIndex(s.characters.startIndex) // expected-error{{argument type 'String.CharacterView.Index' does not conform to expected type 'Strideable'}}

  acceptsCollectionIndex(s.startIndex)
  acceptsBidirectionalCollectionIndex(s.startIndex)
  acceptsRandomAccessCollectionIndex(s.startIndex) // expected-error{{argument type 'Index' (aka 'String.CharacterView.Index') does not conform to expected type 'Strideable'}}
}
