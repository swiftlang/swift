// RUN: %target-parse-verify-swift

class Base {
  func foo() {}
}

class Sub: Base {
  override func foo() {}
}

func removeOverrides<SomeSub: Sub>(concrete: Sub, generic: SomeSub) {
  _ = concrete.foo()
  _ = generic.foo()
}
