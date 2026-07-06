// RUN: %target-swift-emit-silgen -verify %s

func fooify(p: any P) {
  switch p.foo() {
  case .success:
    break
  }
}

enum Resoult: ~Copyable {
  case success
}

protocol P {
  func foo() -> Resoult
}

