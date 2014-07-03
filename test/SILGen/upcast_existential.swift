// RUN: %swift -emit-silgen %s | FileCheck %s

protocol P {}
protocol Q {}

struct X: P, Q {}

func makePQ() -> protocol<P,Q> { return X() }

func useP(x: P) { }

// CHECK-LABEL: sil @_TF18upcast_existential5PQtoPFT_T_ : $@thin () -> () {
func PQtoP() {
  // CHECK: upcast_existential [take] [[PQ:%.*]]#1 : $*protocol<P, Q> to {{%.*}}#1 : $*P
  // CHECK-NOT: destroy_addr [[PQ]]
  useP(makePQ())
}
