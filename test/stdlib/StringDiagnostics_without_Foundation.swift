// RUN: %target-typecheck-verify-swift

// Positive and negative tests for String index types
func acceptsCollection<I: Collection>(_: I) {}
func acceptsBidirectionalCollection<I: BidirectionalCollection>(_: I) {}
func acceptsRandomAccessCollection<I: RandomAccessCollection>(_: I) {}
// expected-note@-1 {{where 'I' = 'String.UTF8View'}}
// expected-note@-2 {{where 'I' = 'String.UnicodeScalarView'}}
// expected-note@-3 {{where 'I' = 'String.UTF16View'}}
// expected-note@-4 {{where 'I' = 'String'}}

func testStringCollectionTypes(s: String) {
  acceptsCollection(s.utf8)
  acceptsBidirectionalCollection(s.utf8)
  acceptsRandomAccessCollection(s.utf8) // expected-error{{global function 'acceptsRandomAccessCollection' requires that 'String.UTF8View' conform to 'RandomAccessCollection'}}

  // UTF16View is random-access with Foundation, bidirectional without
  acceptsCollection(s.utf16)
  acceptsBidirectionalCollection(s.utf16)
  acceptsRandomAccessCollection(s.utf16) // expected-error{{global function 'acceptsRandomAccessCollection' requires that 'String.UTF16View' conform to 'RandomAccessCollection'}}

  acceptsCollection(s.unicodeScalars)
  acceptsBidirectionalCollection(s.unicodeScalars)
  acceptsRandomAccessCollection(s.unicodeScalars) // expected-error{{global function 'acceptsRandomAccessCollection' requires that 'String.UnicodeScalarView' conform to 'RandomAccessCollection'}}

  acceptsCollection(s)
  acceptsBidirectionalCollection(s)
  acceptsRandomAccessCollection(s) // expected-error{{global function 'acceptsRandomAccessCollection' requires that 'String' conform to 'RandomAccessCollection'}}
}
