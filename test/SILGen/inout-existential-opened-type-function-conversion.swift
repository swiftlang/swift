// RUN: %target-swift-emit-silgen -verify %s

protocol P {
  mutating func foo(_: (Self) -> Void)
}

func foo(x: inout P) {
  x.foo { y in return }
}
REQUIRES: updating_for_owned_noescape
