// RUN: %target-typecheck-verify-swift

protocol Wrapper<T> {
  associatedtype T

  func get()
}

func foo<R>(_: R) -> any Wrapper<R> {}

func test() {
  foo(0).get()
}
