// RUN: %target-typecheck-verify-swift

// rdar://problem/32726044 - shrink reduced domains too far
public protocol P_32726044 {}

extension Int: P_32726044 {}
extension Float: P_32726044 {}

public func *(lhs: P_32726044, rhs: P_32726044) -> Double {
  fatalError()
}

func rdar32726044() -> Float {
  var f: Float = 0
  f = Float(1) * 100 // Ok
  let _: Float = Float(42) + 0 // Ok
  return f
}
