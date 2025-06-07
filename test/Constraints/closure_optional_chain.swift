// RUN: %target-swift-frontend -emit-sil %s

struct S {
  func foo<R>(_ body: () -> R) -> R {
    fatalError()
  }
}

// Make sure we insert an implicit return at the end of the body.
func bar(x: S?) {
  x?.foo {
    if .random() { return }
  }
}
