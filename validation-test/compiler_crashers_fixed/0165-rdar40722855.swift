// RUN: %target-typecheck-verify-swift

postfix operator %
postfix func % (_: Any) {}

prefix operator ~
prefix func ~ (_: Any) {}

func foo(_: String) -> Void {}
func foo(_: Int) -> Void {}

_ = foo("answer")% // Ok
_ = ~foo(42)       // Ok

class A {
  func bar(_: Int) {}
}

extension A {
  func bar(_ qux: String) {
    bar(qux)% // Ok
    ~bar(qux) // Ok
  }
}
