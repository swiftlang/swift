// RUN: %target-typecheck-verify-swift

infix operator =*=
protocol P1 {
  static func =*= (lhs: Self, rhs: Self) -> Bool
}
extension P1 {
  static func =*= (lhs: Self, rhs: Self) -> Bool { true }
}
func test1() {
  struct S: P1 {}
}

infix operator =**=
protocol P2 {
  static func =**= (lhs: Self, rhs: Self) -> Bool
}
func test2() {
  struct S: P2 {
    static func =**= (lhs: S, rhs: S) -> Bool { true }
  }
}

func test3() {
  enum E: Equatable {
    case a, b

    static func == (lhs: E, rhs: E) -> Bool { true }
  }
}

func test4() {
  struct Outer {
    enum Inner: Int, Comparable {
      case a, b

      static func < (lhs: Inner, rhs: Inner) -> Bool { true }
    }
  }
}

func test5() {
  class C: Equatable {
    static func == (lhs: C, rhs: C) -> Bool { true }
  }
}
