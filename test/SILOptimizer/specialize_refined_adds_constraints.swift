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

// CHECK-LABEL: sil shared @_T035specialize_refined_adds_constraints1gyxAA1RRzlFAA1XV_Tg5 :
func test(x: X) {
  g(x)
}
