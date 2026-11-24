// rdar://192173225
// RUN: %target-typecheck-verify-swift

protocol P {}
struct S: P {
  static var c: S { S() }
}
extension P where Self == S { static var c: S { S() } }
@resultBuilder struct B {
  static func buildBlock<T>(_ x: T) {}
}
func f<D: Sequence, C>(_ d: D, @B _ c: (D.Element) -> C) {}
func g<R>(_ c: () -> R) -> R { c() }
protocol Q { associatedtype E }
extension Q { func r(_ i: Int) -> E { fatalError() } }
struct A<E>: Q {
  func r(_ i: Int) -> E { fatalError() }
}
@B func h() { // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  f(0..<0 as Range<Int>) { i in
    ({ g { A<Int>().r(i) } } as () -> Void, S.c)
  }
  ()
}
