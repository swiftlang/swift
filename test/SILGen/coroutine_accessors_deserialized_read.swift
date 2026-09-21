// A '_read' accessor read from a .swiftinterface (or any deserialized/SIL input)
// is NOT remapped to 'yielding borrow' -- it stays a genuine 'Read' accessor.
// This checks that such an accessor flows correctly through
// OpaqueReadOwnershipRequest in a feature-ON consumer: it takes the 'Owned'
// default (so a getter is available) and the read still resolves to the 'read'
// (yield_once) coroutine the producer exported.
//
// The interface is a literal fixture rather than one generated from source: a
// plain, unguarded '_read' is what a *pre-feature* module shipped, and once
// CoroutineAccessors is enabled by default it can no longer be produced from
// source (feature-on printing guards it under '#if $CoroutineAccessors' and a
// consumer would pick the 'yielding borrow' branch).  Embedding the interface
// keeps the 'Read'-provenance path tested after the feature is on by default.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build a binary module from the literal interface (feature ON, mirroring the
// post-switch world), so the '_read' is deserialized and stays a 'Read'
// accessor.
// RUN: %target-swift-frontend                                \
// RUN:     -compile-module-from-interface                    \
// RUN:     %t/LibOld.swiftinterface                          \
// RUN:     -o %t/LibOld.swiftmodule                          \
// RUN:     -module-name LibOld                               \
// RUN:     -enable-experimental-feature CoroutineAccessors

// A feature-ON client reading the deserialized '_read' property resolves to the
// 'read' coroutine the producer exported (Sivr, not the remapped Sivy).
// RUN: %target-swift-emit-silgen                             \
// RUN:     %t/Client.swift                                   \
// RUN:     -I %t                                             \
// RUN:     -module-name Client                               \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:   | %FileCheck %t/Client.swift

// REQUIRES: swift_feature_CoroutineAccessors
// The old-ABI-provenance guarantee this test checks only applies on
// ABI-stable platforms; non-ABI-stable platforms always call the new ABI.
// REQUIRES: swift_stable_abi

//--- LibOld.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -swift-version 5 -module-name LibOld
import Swift
public struct S {
  public init()
  public var x: Swift.Int {
    _read
  }
}

//--- Client.swift

import LibOld

@_silgen_name("readX")
func readX(_ s: S) -> Int {
// CHECK-LABEL: sil {{.*}}@readX : {{.*}} {
                  // function_ref S.x.read
// CHECK:         function_ref @$s6LibOld1SV1xSivr : $@yield_once
// CHECK-LABEL: } // end sil function 'readX'
  return s.x
}
