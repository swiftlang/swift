// RUN: %target-swift-frontend -emit-silgen -verify %s

struct S {
  func foo() -> Int {
    // Make sure the decl context for the autoclosure passed to ?? is deep
    // enough that it can 'see' the capture of $0 from the outer closure.
    lazy var nest: (Int) -> Int = { Optional<Int>.none ?? $0 }
    return nest(1)
  }
}

extension S {
  func bar() -> Int {
    lazy var nest: (Int) -> Int = { Optional<Int>.none ?? $0 }
    return nest(1)
  }
}
