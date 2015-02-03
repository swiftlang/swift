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

func testSomeConversionsWork() {
  let xs = "abc"
  xs == "def"
  xs != "def"
  "def" == xs
  "def" != xs
  "abc" == "def"
  "abc" != "def"
  xs == xs
  xs != xs
  let ys: NSString = "abc"
  ys == "def"
  ys != "def"
  "def" == ys
  "def" != ys
  xs == ys
  xs != ys
  ys == ys
  ys != ys
}
