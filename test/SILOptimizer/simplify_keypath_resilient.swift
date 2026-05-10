// RUN: %target-swift-frontend -primary-file %s -enable-library-evolution -emit-sil | %FileCheck %s

public enum E: Hashable {
  case e
}

public struct S {
  public var dict: [E: Int] = [:]
}

// Check that the keypath simplification does not crash when inserting a compensating destroy.

// CHECK-LABEL: sil @$s26simplify_keypath_resilient6testityyF :
// CHECK-NOT:     keypath
// CHECK:       } // end sil function '$s26simplify_keypath_resilient6testityyF'
@inlinable
public func testit() {
  let _ = \S.dict[.e]
}


