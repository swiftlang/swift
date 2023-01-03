// RUN: %target-swift-frontend -enable-experimental-move-only -o - -emit-silgen %s | %FileCheck %s

class Klass {
    func useKlass() {}
}

struct Struct {
    var k = Klass()
}

func useKlass(_ k: Klass) {}

// CHECK-LABEL: sil hidden [ossa] @$s11borrow_expr10simpleTestyyF : $@convention(thin) () -> () {
// CHECK: [[ADDR:%.*]] = project_box
//
// First check without the borrow:
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*Struct, #Struct.k
// CHECK: [[VAL:%.*]] = load [copy] [[GEP]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[FUNC:%.*]] = function_ref @$s11borrow_expr8useKlassyyAA0D0CF : $@convention(thin) (@guaranteed Klass) -> ()
// CHECK: apply [[FUNC]]([[VAL]])
// CHECK: destroy_value [[VAL]]
//
// Now with the borrow:
// CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[ADDR]]
// CHECK: [[GEP:%.*]] = struct_element_addr [[ACCESS]] : $*Struct, #Struct.k
// CHECK: [[VAL:%.*]] = load_borrow [[GEP]]
// CHECK: [[FUNC:%.*]] = function_ref @$s11borrow_expr8useKlassyyAA0D0CF : $@convention(thin) (@guaranteed Klass) -> ()
// CHECK: apply [[FUNC]]([[VAL]])
// CHECK: end_borrow [[VAL]]
// CHECK: end_access [[ACCESS]]
// CHECK: } // end sil function '$s11borrow_expr10simpleTestyyF'
func simpleTest() {
    var s = Struct()
    s = Struct()
    // Without borrow.
    useKlass(s.k)
    // With borrow.
    useKlass(_borrow s.k)
}
