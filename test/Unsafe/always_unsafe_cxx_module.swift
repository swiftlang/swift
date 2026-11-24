// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The C++ overlay can use '@unsafe(always)' without the experimental feature,
// so this does not require it to be available in this compiler.
// RUN: %target-swift-frontend -emit-module -o %t/Cxx.swiftmodule -emit-module-interface-path %t/Cxx.swiftinterface -enable-library-evolution -swift-version 5 -module-name Cxx %t/Cxx.swift
// RUN: %FileCheck %s --input-file %t/Cxx.swiftinterface

// The attribute survives rebuilding the overlay from its interface, which is
// not built with the feature enabled either.
// RUN: rm %t/Cxx.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/client.swift

// CHECK-NOT: AlwaysUnsafeAttribute
// CHECK: #if compiler(>=5.3) && $AlwaysUnsafeAttribute
// CHECK: @unsafe(always) public func alwaysUnsafeFunc()
// CHECK: #else
// CHECK: @unsafe public func alwaysUnsafeFunc()
// CHECK: #endif

//--- Cxx.swift
@unsafe(always)
public func alwaysUnsafeFunc() { }

//--- client.swift
import Cxx

func test() {
  alwaysUnsafeFunc()
  // expected-error@-1{{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}{{documentation-file=always-unsafe}}
  // expected-note@-2{{reference to unsafe global function 'alwaysUnsafeFunc()'}}

  unsafe alwaysUnsafeFunc()
}
