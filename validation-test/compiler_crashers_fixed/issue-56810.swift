// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/56810

protocol Foo {}

func withTrailingClosure(x: (Int) -> Void) {}

_ = withTrailingClosure { (x) in
  extension Foo {
    func foo() {
      _ = MemoryLayout<Self>.size
    }
  }
}
