// RUN: %target-swift-emit-silgen -enable-sil-ownership -enforce-exclusivity=checked %s | %FileCheck %s

class C {}

struct A {}
struct B { var owner: C }

var a = A()

// CHECK-LABEL: sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: assign {{%.*}} to {{%.*}} : $*A
// CHECK: destroy_value {{%.*}} : $B
// CHECK: } // end sil function 'main'
(a, _) = (A(), B(owner: C()))

class D { var child: C = C() }

// Verify that the LHS is formally evaluated before the RHS.
// CHECK-LABEL: sil hidden @$s10assignment5test1yyF : $@convention(thin) () -> () {
func test1() {
  // CHECK: [[T0:%.*]] = metatype $@thick D.Type
  // CHECK: [[CTOR:%.*]] = function_ref @$s10assignment1DC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[D:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[T0:%.*]] = metatype $@thick C.Type
  // CHECK: [[CTOR:%.*]] = function_ref @$s10assignment1CC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[C:%.*]] = apply [[CTOR]]([[T0]]) : $@convention(method) (@thick C.Type) -> @owned C
  // CHECK: [[SETTER:%.*]] = class_method [[D]] : $D,  #D.child!setter.1
  // CHECK: apply [[SETTER]]([[C]], [[D]])
  // CHECK: destroy_value [[D]]
  D().child = C()
}

// rdar://32039566
protocol P {
  var left: Int {get set}
  var right: Int {get set}
}

// Verify that the access to the LHS does not begin until after the
// RHS is formally evaluated.
// CHECK-LABEL: sil hidden @$s10assignment15copyRightToLeft1pyAA1P_pz_tF : $@convention(thin) (@inout P) -> () {
func copyRightToLeft(p: inout P) {
  // CHECK: bb0(%0 : @trivial $*P):
  // CHECK:   [[READ:%.*]] = begin_access [read] [unknown] %0 : $*P
  // CHECK:   [[READ_OPEN:%.*]] = open_existential_addr immutable_access [[READ]]
  // CHECK:   end_access [[READ]] : $*P
  // CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*P
  // CHECK:   [[WRITE_OPEN:%.*]] = open_existential_addr mutable_access [[WRITE]]
  // CHECK:   end_access [[WRITE]] : $*P
  p.left = p.right
}
