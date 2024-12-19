// RUN: %target-typecheck-verify-swift -enable-experimental-feature AllowUnsafeAttribute -enable-experimental-feature WarnUnsafe -I %S/Inputs

// REQUIRES: swift_feature_AllowUnsafeAttribute
// REQUIRES: swift_feature_WarnUnsafe

import unsafe_decls

// expected-warning@+1{{global function 'testUnsafe' involves unsafe code; use '@unsafe' to indicate that its use is not memory-safe}}{{1-1=@unsafe }}
func testUnsafe(_ ut: UnsafeType) { // expected-note{{reference to unsafe struct 'UnsafeType'}}
  unsafe_c_function() // expected-note{{call to unsafe global function 'unsafe_c_function()'}}

  var array: [CInt] = [1, 2, 3, 4, 5]
  print_ints(&array, CInt(array.count))
  // expected-note@-1{{call to global function 'print_ints' involves unsafe type 'UnsafeMutablePointer<Int32>'}}
}
