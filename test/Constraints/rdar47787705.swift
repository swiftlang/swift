// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

protocol P {
  init(value: Int)
}

class C {
  init(value: Int, otherValue: String = "") {}
}

func make<T: P & C>(type: T.Type) -> T {
  // CHECK: [[INIT:.*]] = witness_method $T, #P.init
  return T.init(value: 0)
}
