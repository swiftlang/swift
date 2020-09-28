// RUN: %target-swift-frontend -emit-sil %s -O | %FileCheck %s

extension FloatingPoint {
  public static var expOverflowThreshold: Self {
    return Self(Self.greatestFiniteMagnitude.exponent)
  }
}

// CHECK-LABEL: sil @$s20bottomuporder_update3fooSfyF :
// CHECK-NOT: apply
// CHECK-LABEL: } // end sil function '$s20bottomuporder_update3fooSfyF'
public func foo() -> Float {
  return Float.expOverflowThreshold
}

public func bar() -> Float {
  return Float(Float.greatestFiniteMagnitude.exponent)
}

