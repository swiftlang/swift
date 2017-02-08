// RUN: %target-swift-frontend -typecheck %s -verify

// REQUIRES: objc_interop

import Darwin
import ObjectiveC

errno = 0
assert(errno == 0)

fork() // expected-error{{'fork()' is unavailable}} expected-warning {{result of call to 'fork()' is unused}}
vfork() // expected-error{{'vfork()' is unavailable}} expected-warning {{result of call to 'vfork()' is unused}}


// Test YES and NO.
let x_YES = YES // expected-error {{'YES' is unavailable: Use 'Bool' value 'true' instead}}
let x_NO = NO // expected-error {{'NO' is unavailable: Use 'Bool' value 'false' instead}}

func test_shadow(_ flag: Bool) -> Bool {
  let YES = true
  let NO = false
  return flag ? YES : NO
}

