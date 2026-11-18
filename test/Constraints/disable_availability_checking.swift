// RUN: %target-typecheck-verify-swift

struct S {
  @available(swift, obsoleted: 4.2)
  init(foo: Int) throws {}

  @available(swift, introduced: 4.2)
  init?(foo: Int) throws {}
}

_ = try S(foo: 42)
