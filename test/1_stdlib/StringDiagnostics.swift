// RUN: %target-parse-verify-swift

// XFAIL: linux

import Foundation

// Common pitfall: trying to subscript a string with integers.
func testIntSubscripting(s: String, i: Int) {
  let c1 = s[i] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c2 = s[17] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c3 = s[i...i] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c4 = s[17..<20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c5 = s[17...20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
}

func testNonAmbiguousStringComparisons() {
  let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]
  var reversed1 = sorted(names, { s1, s2 in s1 > s2 })
  var reversed2 = sorted(names, { s1, s2 in s1 as String > s2 })
}

func testAmbiguousStringComparisons(s: String) {
  let nsString = s as NSString
  let a1 = s == nsString
  let a2 = s != nsString
  let a3 = s < nsString // expected-error {{binary operator '<' cannot be applied to operands of type 'String' and 'NSString'}} expected-note {{overloads for '<' exist with these partially matching parameter lists: (String, String)}}
  let a4 = s <= nsString // expected-error {{binary operator '<=' cannot be applied to operands of type 'String' and 'NSString'}}
  let a5 = s >= nsString // expected-error {{binary operator '>=' cannot be applied to operands of type 'String' and 'NSString'}}
  let a6 = s > nsString // expected-error {{binary operator '>' cannot be applied to operands of type 'String' and 'NSString'}}
  // now the other way
  let a7 = nsString == s
  let a8 = nsString != s
  let a9 = nsString < s // expected-error {{binary operator '<' cannot be applied to operands of type 'NSString' and 'String'}} expected-note {{overloads for '<' exist with these partially matching parameter lists: (String, String)}} 
  let a10 = nsString <= s // expected-error {{binary operator '<=' cannot be applied to operands of type 'NSString' and 'String'}}
  let a11 = nsString >= s // expected-error {{binary operator '>=' cannot be applied to operands of type 'NSString' and 'String'}}
  let a12 = nsString > s // expected-error {{binary operator '>' cannot be applied to operands of type 'NSString' and 'String'}}
}
