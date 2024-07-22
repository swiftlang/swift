// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature DisallowUnsafe -I %S/Inputs

import unsafe_decls

func testUnsafe(_ ut: UnsafeType) { // expected-error{{reference to unsafe struct 'UnsafeType'}}
  unsafe_c_function() // expected-error{{call to unsafe global function 'unsafe_c_function'}}
}
