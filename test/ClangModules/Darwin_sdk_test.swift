// RUN: %target-swift-frontend -parse %s -verify

// REQUIRES: objc_interop

import Darwin
import ObjectiveC

errno = 0
assert(errno == 0)

fork() // expected-error{{'fork()' is unavailable}}
vfork() // expected-error{{'vfork()' is unavailable}}


// Test YES and NO.
let x_YES = YES // expected-error {{'YES' is unavailable: Use 'Bool' value 'true' instead}}
let x_NO = NO // expected-error {{'NO' is unavailable: Use 'Bool' value 'false' instead}}

func test_shadow(flag: Bool) -> Bool {
  let YES = true
  let NO = false
  return flag ? YES : NO
}

