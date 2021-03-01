extension S {
  static func foo<I: SignedInteger>(_ si: I) {
    print("1: other:2 not commented out")
  }
  static func foo2<I: SignedInteger>(_ si: I) {
    print("2: other:6 not commented out")
  }
}

S.foo(3)
S.foo2(3)
