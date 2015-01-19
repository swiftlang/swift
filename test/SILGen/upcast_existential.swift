// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

protocol P {}
protocol Q {}

struct X: P, Q {}

func makePQ() -> protocol<P,Q> { return X() }

func useP(x: P) { }

// CHECK-LABEL: sil hidden @_TF18upcast_existential5PQtoPFT_T_ : $@thin () -> () {
func PQtoP() {
  // CHECK: [[PQ_PAYLOAD:%.*]] = open_existential [[PQ:%.*]]#1 : $*protocol<P, Q> to $*[[OPENED_TYPE:@opened(.*) protocol<P, Q>]]
  // CHECK: [[P_PAYLOAD:%.*]] = init_existential [[P:%.*]]#1 : $*P, $[[OPENED_TYPE]]
  // CHECK: copy_addr [take] [[PQ_PAYLOAD]] to [initialization] [[P_PAYLOAD]]
  // CHECK-NOT: destroy_addr [[P]]
  // CHECK-NOT: destroy_addr [[P_PAYLOAD]]
  // CHECK-NOT: destroy_addr [[PQ]]
  // CHECK-NOT: destroy_addr [[PQ_PAYLOAD]]
  useP(makePQ())
}
