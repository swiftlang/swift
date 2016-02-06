// RUN: %target-parse-verify-swift

// XFAIL: linux

import Foundation

// Common pitfall: trying to subscript a string with integers.
func testIntSubscripting(s: String, i: Int) {
  _ = s[i] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int, see the documentation comment for discussion}}
  _ = s[17] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int, see the documentation comment for discussion}}
  _ = s[i...i] // expected-error{{subscript' is unavailable: cannot subscript String with a Range<Int>, see the documentation comment for discussion}}
  _ = s[17..<20] // expected-error{{subscript' is unavailable: cannot subscript String with a Range<Int>, see the documentation comment for discussion}}
  _ = s[17...20] // expected-error{{subscript' is unavailable: cannot subscript String with a Range<Int>, see the documentation comment for discussion}}
}

// Common pitfall: trying to access `String.count`.
func testStringCount(s: String) {
  _ = s.count // expected-error{{'count' is unavailable: there is no universally good answer, see the documentation comment for discussion}}
}

func testNonAmbiguousStringComparisons() {
  let s1 = "a"
  let s2 = "b"
  var x = false // expected-warning {{variable 'x' was written to, but never read}}
  x = s1 > s2
  x = s1 as String > s2
}

func testAmbiguousStringComparisons(s: String) {
  let nsString = s as NSString
  let a1 = s == nsString
  let a2 = s != nsString
  let a3 = s < nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{24-24= as String}}
  let a4 = s <= nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{25-25= as String}}
  let a5 = s >= nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{25-25= as String}}
  let a6 = s > nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{24-24= as String}}
  // now the other way
  let a7 = nsString == s
  let a8 = nsString != s
  let a9 = nsString < s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{20-20= as String}}
  let a10 = nsString <= s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{21-21= as String}}
  let a11 = nsString >= s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{21-21= as String}}
  let a12 = nsString > s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}} {{21-21= as String}}
}

func acceptsSequence<S : SequenceType>(sequence: S) {}

func testStringIsNotASequence(s: String) {
  acceptsSequence(s) // expected-error {{argument type 'String' does not conform to expected type 'SequenceType'}}
}

func testStringDeprecation(hello: String) {
  let hello2 = hello
    .stringByAddingPercentEscapesUsingEncoding(NSUTF8StringEncoding) // expected-warning{{'stringByAddingPercentEscapesUsingEncoding' is deprecated}}

  _ = hello2?
    .stringByReplacingPercentEscapesUsingEncoding(NSUTF8StringEncoding) // expected-warning{{'stringByReplacingPercentEscapesUsingEncoding' is deprecated}}


}

// Positive and negative tests for String index types
func acceptsForwardIndex<I: ForwardIndexType>(index: I) {}
func acceptsBidirectionalIndex<I: BidirectionalIndexType>(index: I) {}
func acceptsRandomAccessIndex<I: RandomAccessIndexType>(index: I) {}

func testStringIndexTypes(s: String) {
  acceptsForwardIndex(s.utf8.startIndex)
  acceptsBidirectionalIndex(s.utf8.startIndex) // expected-error{{argument type 'String.UTF8View.Index' does not conform to expected type 'BidirectionalIndexType'}}
  acceptsBidirectionalIndex(s.unicodeScalars.startIndex)
  acceptsRandomAccessIndex(s.unicodeScalars.startIndex) // expected-error{{argument type 'String.UnicodeScalarView.Index' does not conform to expected type 'RandomAccessIndexType'}}
  acceptsBidirectionalIndex(s.characters.startIndex)
  acceptsRandomAccessIndex(s.characters.startIndex) // expected-error{{argument type 'String.CharacterView.Index' does not conform to expected type 'RandomAccessIndexType'}}
  
  // UTF16View.Index is random-access with Foundation, bidirectional without
  acceptsRandomAccessIndex(s.utf16.startIndex)
}

