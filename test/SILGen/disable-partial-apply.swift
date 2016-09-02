// RUN: %target-swift-frontend -emit-silgen -disable-sil-partial-apply %s | %FileCheck %s

// TODO: check strings

protocol P {
  func pMethod()
  static func spMethod()
}

struct S {
  var x: Int
  func sMethod() {}
  static func ssMethod() {}
}

struct G<T> {
  var x: T
  func gMethod() {}
  static func sgMethod() {}
}

class C {
  func cMethod() {}
  static func scMethod() {}
}

func useClosure(_: () -> ()) {}

func foo(p: P, s: S, g: G<P>, c: C) {
  func inner() {
    _ = p
    _ = s
    _ = g
    _ = c
  }

  useClosure(inner)
  useClosure(p.pMethod)
  useClosure(type(of: p).spMethod)
  useClosure(s.sMethod)
  useClosure(type(of: s).ssMethod)
  useClosure(c.cMethod)
  useClosure(type(of: c).scMethod)

  useClosure {[unowned c] in
    _ = c
  }
  useClosure {[weak c] in ()
    _ = c
  }
}
