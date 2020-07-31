// RUN: %target-swift-frontend -typecheck -verify %s

public protocol MyProtocol {}

public struct MyProtocolImpl: MyProtocol {}

public func != (lhs: MyProtocol, rhs: MyProtocol) -> MyProtocolImpl {
  return MyProtocolImpl()
}

public func && (lhs: MyProtocol, rhs: MyProtocol) -> MyProtocolImpl {
  return MyProtocolImpl()
}

func check(a: Double, b: Int64) -> Bool {
  return a != 0 && b != 0 // Okay
}

func check() {
  let x: Int = 1
  let _ = UInt(1) << x - 1 // Okay
  let _ = UInt(1) << (x + 1) - 1 // Okay
}
