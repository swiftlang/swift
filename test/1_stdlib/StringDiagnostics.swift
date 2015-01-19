// RUN: %target-parse-verify-swift

import Foundation

// Common pitfall: trying to subscript a string with integers.
func testIntSubscripting(s: String, i: Int) {
  let c1 = s[i] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c2 = s[17] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c3 = s[i...i] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c4 = s[17..<20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c5 = s[17...20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
}

func testAmbiguousStringComparisons(s: String) {
  let nsString = s as NSString
  let a1 = s == nsString // expected-error{{'==' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a2 = s != nsString // expected-error{{'!=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a3 = s < nsString // expected-error{{'<' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a4 = s <= nsString // expected-error{{'<=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a5 = s >= nsString // expected-error{{'>=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a6 = s > nsString // expected-error{{'>' is unavailable: Comparing Swift.String and NSString is ambiguous}}
}
