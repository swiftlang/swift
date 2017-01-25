// RUN: %target-swift-frontend  -O -sil-inline-threshold 0 -emit-sil -primary-file %s | %FileCheck %s

protocol P { }

protocol Q {
  associatedtype Assoc
  func assoc() -> Assoc
}

protocol R : Q {
  associatedtype Assoc: P
}

func f<A: P>(_: A) { }

func g<T: R>(_ t: T) {
  f(t.assoc())
}

struct X : R {
  struct Assoc: P { }
  func assoc() -> Assoc { return Assoc() }
}

// CHECK-LABEL: sil shared @_TTSg5V35specialize_refined_adds_constraints1XS0_S_1RS____TF35specialize_refined_adds_constraints1guRxS_1RrFxT_ :
func test(x: X) {
  g(x)
}
