// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module-path=%t/ResilientBase.swiftmodule -module-name=ResilientBase %S/Inputs/ResilientBase.swift
// RUN: %target-swift-frontend -primary-file %s -emit-ir -I %t -profile-generate -profile-coverage-mapping

// This test use to crash because we emitted a FieldAccess::NonConstantDirect for _t.

import ResilientBase

class TheTest {
  func test_resilientSubclassCrasher() {
    final class SubclassOfResilientBase<T> : ResilientBase {
      @Wrapper var t: T

      init(_ t: T) {
        self.t = t
      }
    }
    _ = SubclassOfResilientBase(0)
  }
}
