// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s | %FileCheck %s

// rdar://20532214 -- Wrong code for witness method lookup lets executable crash

public protocol P1 {
  func foo(_ rhs: Self) -> Bool
}

public protocol P2 {
  associatedtype Index : P1

  var startIndex: Index {get}
}

public protocol P3 : P1 {}

public struct C3 : P3 {
  public func foo(_ rhs: C3) -> Bool {
    return true
  }
}

public struct C2 : P2 {
  public var startIndex: C3 {
    return C3()
  }
}

extension P2 where Self.Index : P3 {
  public var bar: Bool {
	  let i = startIndex
    return i.foo(i)
  }
}

let s = C2()

s.bar

// CHECK: call {{.*}} @"$s29protocol_extensions_constrain2C3VAA2P3AAWP"
