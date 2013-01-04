// RUN: %swift -parse -verify -constraint-checker %s
// XFAIL: *
// See: <rdar://problem/12959759> Constrait checker cannot handle...
protocol P {
  static func foo(arg : This) -> This
}
struct S : P {
  static func foo(s : S) -> S {
    return s
  }
}

func foo<T : P>(arg : T) -> T {
  return T.foo(arg)
}
