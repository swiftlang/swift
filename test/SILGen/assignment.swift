// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class C {}

struct A {}
struct B { var owner: C }

var a = A()

// CHECK: assign {{%.*}} to {{%.*}} : $*A
// CHECK: destroy_value {{%.*}} : $B
(a, _) = (A(), B(owner: C()))

class D { var child: C = C() }

// Verify that the LHS is formally evaluated before the RHS.
// CHECK: sil hidden @_T010assignment5test1yyF : $@convention(thin) () -> () {
func test1() {
  // CHECK: [[CTOR:%.*]] = function_ref @_T010assignment1DC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[T0:%.*]] = metatype $@thick D.Type
  // CHECK: [[D:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[CTOR:%.*]] = function_ref @_T010assignment1CC{{[_0-9a-zA-Z]*}}fC
  // CHECK: [[T0:%.*]] = metatype $@thick C.Type
  // CHECK: [[C:%.*]] = apply [[CTOR]]([[T0]])
  // CHECK: [[SETTER:%.*]] = class_method [[D]] : $D,  #D.child!setter.1
  // CHECK: [[BORROWED_D:%.*]] = begin_borrow [[D]]
  // CHECK: apply [[SETTER]]([[C]], [[BORROWED_D]])
  // CHECK: end_borrow [[BORROWED_D]] from [[D]]
  D().child = C()
}
