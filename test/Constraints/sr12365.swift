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

func check1() {
  let x: Int = 1
  let _ = UInt(1) << x - 1 // Okay
  let _ = UInt(1) << (x + 1) - 1 // Okay
}

func check2() {
  let a: UInt32 = 0
  let b: UInt32 = 1 << (a + 16)
  let _ = a & b // Okay
}

func check3() {
  let a: UInt32 = 0
  let b = 1 << (a + 16)
  let _ = a & b // Not okay, because 'b: Int'!
  // expected-error@-1 {{binary operator '&' cannot be applied to operands of type 'UInt32' and 'Int'}}
  // expected-note@-2 {{overloads for '&' exist with these partially matching parameter lists: (Int, Int), (UInt32, UInt32)}}
}
