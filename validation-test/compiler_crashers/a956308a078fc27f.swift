// https://github.com/swiftlang/swift/issues/84884
// RUN: not --crash %target-swift-frontend -typecheck %s

protocol Proto<Assoc> {
  associatedtype Assoc
}

struct Generic<Assoc>: Proto {
  static func foo(_: Generic<Assoc>) -> Generic<Assoc> {}
  static func bar(_: some Proto<Assoc>) -> Generic<Assoc> {}
}

struct S1 {}
struct S2 {}

extension Proto {
  func overload() -> some Proto<S1> where Assoc == S1 {
    Generic()
  }
  func overload() -> some Proto<S1> where Assoc == Generic<Generic<S1>> {
    Generic()
  }
}

struct Struct: Proto {
  typealias Assoc = Generic<Generic<S2>>
  init(_: Int) {}
}

func test() {
  let _ = Generic.foo(Generic.bar(Struct(0).overload())
}
