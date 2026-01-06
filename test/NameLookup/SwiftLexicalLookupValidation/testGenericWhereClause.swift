// REQUIRES: swift_feature_UnqualifiedLookupValidation
//
// RUN: %target-typecheck-verify-swift -enable-experimental-feature UnqualifiedLookupValidation

protocol P1 {
    associatedtype A
    func f() -> A
}

protocol P2 {
    associatedtype A: P2
    associatedtype B: P2 where Self.A.A == Self.B.A
}

protocol P3 {
    associatedtype A: P3
}

struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}

struct Recur: P2 {
    typealias A = Recur
    typealias B = Recur
}

struct NonRecur: P2 {
    typealias A = Recur
    typealias B = Recur
}

struct Generic<T> {}

class Super<T, U> {}

extension Super: P2 where T: P2, U: P2 {
    typealias A = T
    typealias B = T
  
    func foo() -> Int { fatalError() }
}

class Sub: Super<NonRecur, Recur> {}

struct RecurGeneric<T: P3>: P3 {
    typealias A = RecurGeneric<T>
}

struct Specialize: P3 {
    typealias A = RecurGeneric<Specialize>
}

protocol P48a { associatedtype A = Int }
protocol P48b { associatedtype B }
protocol P48c: P48a, P48b where A == B {}

public extension Array where Element == Int {
  mutating func foo(
    at index: Int,
    byCalling closure: (inout Element) -> Void
  ) where Element: Differentiable { // expected-error{{cannot find type 'Differentiable' in scope}}
    closure(&self[index])
  }
}

public extension Array {
  mutating func bar(
    at index: Int,
    byCalling closure:(inout Element) -> Void
  ) where Element: Differentiable { // expected-error{{cannot find type 'Differentiable' in scope}}
    closure(&self[index])
  }
}
