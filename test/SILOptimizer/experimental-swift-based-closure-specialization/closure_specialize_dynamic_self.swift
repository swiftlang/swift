// RUN: %target-swift-frontend -emit-sil -O -experimental-swift-based-closure-specialization -primary-file %s

// Just make sure we skip the optimization and not crash here.
//
// Eventually, we can make this work.
//
// <rdar://problem/31725007>

class Foo {
  required init() {}

  static func foo(_ f: () -> ()) -> Self {
    f()
    return self.init()
  }
}

class Bar: Foo {}

func closures(_ x: String) {
  print(Foo.foo { _ = x })
  print(Bar.foo { _ = x })
}
