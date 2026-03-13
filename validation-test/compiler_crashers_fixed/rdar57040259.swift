// RUN: not %target-swift-frontend -typecheck %s
class A { }
class B: A {
  func foo(_: () -> ()) {

  override var prop: Any? {
      didSet { }
  }
}
