// RUN: %target-parse-verify-swift

// XFAIL: *

import Foundation

func testAmbiguousStringComparisons(s: String) {
  let nsString = s as NSString
  let a1 = s == nsString // expected-error{{'==' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a2 = s != nsString // expected-error{{'!=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a3 = s < nsString // expected-error{{'<' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a4 = s <= nsString // expected-error{{'<=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a5 = s >= nsString // expected-error{{'>=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a6 = s > nsString // expected-error{{'>' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  // now the other way
  let a7 = nsString == s // expected-error{{'==' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a8 = nsString != s // expected-error{{'!=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a9 = nsString < s // expected-error{{'<' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a10 = nsString <= s // expected-error{{'<=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a11 = nsString >= s // expected-error{{'>=' is unavailable: Comparing Swift.String and NSString is ambiguous}}
  let a12 = nsString > s // expected-error{{'>' is unavailable: Comparing Swift.String and NSString is ambiguous}}
}
