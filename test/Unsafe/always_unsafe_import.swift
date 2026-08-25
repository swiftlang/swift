// RUN: %empty-directory(%t)

// Build a library that exports always-unsafe declarations, both as a binary
// module and as a module interface.
// RUN: %target-swift-frontend -emit-module -o %t/always_unsafe_lib.swiftmodule -emit-module-interface-path %t/always_unsafe_lib.swiftinterface -enable-library-evolution -swift-version 5 -module-name always_unsafe_lib %S/Inputs/always_unsafe_lib.swift

// The requirement survives serialization into a binary module.
// RUN: %target-typecheck-verify-swift -I %t

// ...and a round trip through a textual interface.
// RUN: rm %t/always_unsafe_lib.swiftmodule
// RUN: %target-typecheck-verify-swift -I %t

import always_unsafe_lib

func test(value: AlwaysUnsafeStruct) {
  alwaysUnsafeFunc()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeFunc()'}}

  unsafe alwaysUnsafeFunc()

  _ = value
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to parameter 'value' involves unsafe type 'AlwaysUnsafeStruct'}}

  _ = unsafe value

  // A merely unsafe import needs no acknowledgement in this language mode.
  unsafeFunc()
}
