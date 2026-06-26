// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -verify %s

protocol P {
  mutating func foo(_: (Self) -> Void)
}

func foo(x: inout P) {
  x.foo { y in return }
}
