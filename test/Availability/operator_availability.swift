// RUN: %target-typecheck-verify-swift -swift-version 4


// rdar://problem/31592529
infix operator <=< : BitwiseShiftPrecedence
infix operator >=> : BitwiseShiftPrecedence

public protocol P {}

extension P {
  public static func <=< <Other : P>(_ x: Self, _ y: Other) { }

  @available(swift, obsoleted: 4)
  public static func >=> <Other : P>(_ x: Self, _ y: Other) { }
}

extension Int : P {}
extension Int32 : P {}

extension Int32 {
  @available(swift, obsoleted: 4)
  public static func <=< (_ x: Int32, _ y: Int32) {}

  @available(swift, obsoleted: 4)
  public static func >=> (_ x: Int32, _ y: Int32) {} // expected-note{{'>=>' was obsoleted in Swift 4}}
}

func testAvailability() {
  _ = (1 as Int32) <=< (1 as Int32)   // okay
  _ = (1 as Int32) >=> (1 as Int32)   // expected-error{{'>=>' is unavailable}}
}

// rdar://problem/152700122
infix operator ~>
public func ~><T> (lhs: T, rhs: (T) -> Void) -> T {
  fatalError()
}

struct S {
  @available(macOS 50, *)
  func f() {}
}

let s = S() ~> {
  if #available(macOS 50.0, *) {
    $0.f()
  }
}
