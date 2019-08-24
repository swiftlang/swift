// RUN: not %target-typecheck-verify-swift

struct Foo {
  public func subscribe(_: @escaping () -> Void) {}
  public static func foo() {}

  func bind() {
    subscribe {
      _ = "\(foo)"
    }
  }
}
