// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %S/Inputs

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

import unsafe_decls

// expected-note@+2 2{{make global function 'testUnsafe' @safe(unchecked) to allow it to use unsafe constructs in its definition}}
// expected-note@+1{{make global function 'testUnsafe' @unsafe to indicate that its use is not memory-safe}}
func testUnsafe(_ ut: UnsafeType) { // expected-warning{{reference to unsafe struct 'UnsafeType'}}
  unsafe_c_function() // expected-warning{{call to unsafe global function 'unsafe_c_function'}}

  var array: [CInt] = [1, 2, 3, 4, 5]
  print_ints(&array, CInt(array.count))
  // expected-warning@-1{{call to global function 'print_ints' involves unsafe type 'UnsafeMutablePointer<Int32>'}}
}
