// RUN: %target-swift-frontend %s -typecheck

class B : Equatable {
  static func == (lhs: B, rhs: B) -> Bool { return true }
}

class C : B {
  static var v: C { return C() }
}

let c: C! = nil
_ = c == .v
