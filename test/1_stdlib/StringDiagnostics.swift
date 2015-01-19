// RUN: %target-parse-verify-swift

// Common pitfall: trying to subscript a string with integers.
func testIntSubscripting(s: String, i: Int) {
  let c1 = s[i] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c2 = s[17] // expected-error{{'subscript' is unavailable: cannot subscript String with an Int}}
  let c3 = s[i...i] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c4 = s[17..<20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
  let c5 = s[17...20] // expected-error{{subscript' is unavailable: cannot subscript String with a range of Int}}
}

