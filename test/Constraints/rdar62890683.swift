// RUN: %target-typecheck-verify-swift

class C {
  init!() {}
}

func foo<T>(_: T.Type, _ fn: () -> T) {}

func test() {
  foo(C.self) { C() }
}
