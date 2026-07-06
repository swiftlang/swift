// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-builtin-module \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

import Builtin
struct NE: ~Escapable {}

// In rdar://157801454, a bug is mentioned where the lifetimes specified by a protocol are copied to the implementation.
// This test verifies that this does not happen.
protocol P157801454 {
  @_lifetime(borrow a1, borrow a2)
  func lifetimetest(a1: Int, a2: Int) -> NE
}

// CHECK-LABEL: sil hidden @$s4test10S157801454V12lifetimetest2a12a2AA2NEVSi_SitF : $@convention(method) (Int, Int, S157801454) -> @lifetime(borrow 1) @owned NE {
// CHECK-LABEL: } // end sil function '$s4test10S157801454V12lifetimetest2a12a2AA2NEVSi_SitF'
struct S157801454 : P157801454 {
  @_lifetime(borrow a2)
  func lifetimetest(a1: Int, a2: Int) -> NE {
    fatalError()
  }
}
