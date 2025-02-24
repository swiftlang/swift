// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

do {
  protocol P {}
  struct S<each T> {}

  func foo<T: P>(_: T, _: Optional<S<T>> = nil) {}

  let p: any P
  foo(p) // OK
}
