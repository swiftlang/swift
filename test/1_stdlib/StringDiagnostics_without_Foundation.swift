// RUN: %target-parse-verify-swift

// Positive and negative tests for String index types
func acceptsForwardIndex<I: ForwardIndex>(index: I) {}
func acceptsBidirectionalIndex<I: BidirectionalIndex>(index: I) {}
func acceptsRandomAccessIndex<I: RandomAccessIndex>(index: I) {}

func testStringIndexTypes(s: String) {
  acceptsForwardIndex(s.utf8.startIndex)
  acceptsBidirectionalIndex(s.utf8.startIndex) // expected-error{{argument type 'String.UTF8View.Index' does not conform to expected type 'BidirectionalIndex'}}
  acceptsBidirectionalIndex(s.unicodeScalars.startIndex)
  acceptsRandomAccessIndex(s.unicodeScalars.startIndex) // expected-error{{argument type 'String.UnicodeScalarView.Index' does not conform to expected type 'RandomAccessIndex'}}
  acceptsBidirectionalIndex(s.characters.startIndex)
  acceptsRandomAccessIndex(s.characters.startIndex) // expected-error{{argument type 'String.CharacterView.Index' does not conform to expected type 'RandomAccessIndex'}}
  
  // UTF16View.Index is random-access with Foundation, bidirectional without
  acceptsBidirectionalIndex(s.utf16.startIndex)
  acceptsRandomAccessIndex(s.utf16.startIndex) // expected-error{{argument type 'String.UTF16View.Index' does not conform to expected type 'RandomAccessIndex'}}
}

