// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/ImplementationOnlyDefs.swiftmodule -module-name ImplementationOnlyDefs %S/Inputs/ImplementationOnlyDefs.swift \
// RUN:   -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -enable-library-evolution -swift-version 5 -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -enable-library-evolution -swift-version 5 -emit-sil -o /dev/null -verify -strict-concurrency=targeted %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -enable-library-evolution -swift-version 5 -emit-sil -o /dev/null -verify -strict-concurrency=complete %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -I %t -enable-library-evolution -swift-version 5 -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_RegionBasedIsolation

@_implementationOnly import ImplementationOnlyDefs
// expected-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}}

class D: C {
  @_implementationOnly
  override func f(_: @escaping () -> Void) { }

  @_implementationOnly
  override func g(_: @escaping () -> Void) -> BSub { BSub() }
  // expected-error@-1{{'@_implementationOnly' override must have the same type as the declaration it overrides ('(@escaping () -> Void) -> BSuper')}}
}
