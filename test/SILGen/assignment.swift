// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class C {}

struct A {}
struct B { var owner: C }

var a = A()

// CHECK: assign {{%.*}} to {{%.*}} : $*A
// CHECK: release_value {{%.*}} : $B
(a, _) = (A(), B(owner: C()))

class D { var child: C = C() }

// Verify that the LHS is formally evaluated before the RHS.
// CHECK: sil hidden @_TF10assignment5test1FT_T_ : $@thin () -> () {
func test1() {
  // CHECK: [[CTOR:%.*]] = function_ref @_TFC10assignment1DCfMS0_FT_S0_
  // CHECK: [[T0:%.*]] = metatype $@thick D.Type
  // CHECK: [[D:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[CTOR:%.*]] = function_ref @_TFC10assignment1CCfMS0_FT_S0_
  // CHECK: [[T0:%.*]] = metatype $@thick C.Type
  // CHECK: [[C:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[SETTER:%.*]] = class_method [[D]] : $D,  #D.child!setter.1
  // CHECK: apply [[SETTER]]([[C]], [[D]])
  D().child = C()
}
