// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ImplementationOnlyDefs.swiftmodule -module-name ImplementationOnlyDefs %S/Inputs/ImplementationOnlyDefs.swift
// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %t

// REQUIRES: concurrency

@_implementationOnly import ImplementationOnlyDefs

class D: C {
  @_implementationOnly
  override func f(_: @escaping () -> Void) { }

  @_implementationOnly
  override func g(_: @escaping () -> Void) -> BSub { BSub() }
  // expected-error@-1{{'@_implementationOnly' override must have the same type as the declaration it overrides ('(@escaping () -> Void) -> BSuper')}}
}
