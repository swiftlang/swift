// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
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

