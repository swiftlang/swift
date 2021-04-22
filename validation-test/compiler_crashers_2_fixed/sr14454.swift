// RUN: not %target-swift-frontend -typecheck %s
protocol Foo {}

func withTrailingClosure(x: (Int) -> Void) {}

_ = withTrailingClosure { (x) in
  extension Foo {
    func foo() {
      _ = MemoryLayout<Self>.size
    }
  }
}
