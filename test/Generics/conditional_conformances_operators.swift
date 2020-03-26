// RUN: %target-typecheck-verify-swift

// rdar://problem/35480952

postfix operator %%%
protocol P {
  static postfix func %%%(lhs: Self)
}
protocol Q {}
struct Foo<T> {}
extension Foo: P where T : Q {
  static postfix func %%%(lhs: Foo<T>) {}
}

func useIt<T: Q>(_: T.Type) {
  Foo<T>()%%%
}
