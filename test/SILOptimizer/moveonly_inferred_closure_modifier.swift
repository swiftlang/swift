// RUN: %target-swift-frontend -emit-sil -verify %s
// 
// rdar://192270068

func test() {
  foo { butt in
    butt.b()
    butt.c()
  }
}

struct Butt: ~Copyable {
  var a: Any = 0 // parameter type needs to be address-only
  borrowing func b() {}
  consuming func c() {}
}

func foo(x: (consuming Butt) -> Void) {}
