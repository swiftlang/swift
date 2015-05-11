// RUN: %target-parse-verify-swift

// XFAIL: linux

import Foundation

// Common pitfall: trying to subscript a string with integers.
func testIntSubscripting(s: String, i: Int) {
  let _ = s[i] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let _ = s[17] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let _ = s[i...i] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let _ = s[17..<20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let _ = s[17...20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
}

func testNonAmbiguousStringComparisons() {
  let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]
  var _ = sorted(names, { s1, s2 in s1 > s2 })
  var _ = sorted(names, { s1, s2 in s1 as String > s2 })
}

func testAmbiguousStringComparisons(s: String) {
  let nsString = s as NSString
  let a1 = s == nsString
  let a2 = s != nsString
  let a3 = s < nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a4 = s <= nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a5 = s >= nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a6 = s > nsString // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  // now the other way
  let a7 = nsString == s
  let a8 = nsString != s
  let a9 = nsString < s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a10 = nsString <= s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a11 = nsString >= s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
  let a12 = nsString > s // expected-error{{'NSString' is not implicitly convertible to 'String'; did you mean to use 'as' to explicitly convert?}}
}

func acceptsSequence<S : SequenceType>(sequence: S) {}

func testStringIsNotASequence(s: String) {
  acceptsSequence(s) // expected-error {{cannot invoke 'acceptsSequence' with an argument list of type '(String)'}} expected-note {{expected an argument list of type '(S)'}}
}

