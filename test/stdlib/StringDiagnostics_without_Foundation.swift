// RUN: %target-typecheck-verify-swift

// Positive and negative tests for String index types
func acceptsCollection<I: Collection>(_: I) {}
func acceptsBidirectionalCollection<I: BidirectionalCollection>(_: I) {}
func acceptsRandomAccessCollection<I: RandomAccessCollection>(_: I) {}

func testStringCollectionTypes(s: String) {
  acceptsCollection(s.utf8)
  acceptsBidirectionalCollection(s.utf8) // expected-error{{argument type 'String.UTF8View' does not conform to expected type 'BidirectionalCollection'}}
  acceptsRandomAccessCollection(s.utf8) // expected-error{{argument type 'String.UTF8View' does not conform to expected type 'RandomAccessCollection'}}

  // UTF16View is random-access with Foundation, bidirectional without
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

struct NotLosslessStringConvertible {}

func testStringInitT() {
  _ = String(NotLosslessStringConvertible()) // expected-error{{'init' has been renamed to 'init(describing:)}}{{14-14=describing: }}
}
