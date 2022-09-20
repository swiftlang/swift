// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P {}
class C {
  init(x: some P) {}

  convenience init(y: any P) { self.init(x: y) }
}

// CHECK-LABEL: sil hidden [ossa] @$s37opened_existential_in_init_delegation1CC1yAcA1P_p_tcfC : $@convention(method) (@in any P, @thick C.Type) -> @owned C {
// CHECK: [[OPENED:%.*]] = open_existential_addr immutable_access %0 : $*any P to $*@opened("{{.*}}", any P) Self
// CHECK: [[COPIED:%.*]] = alloc_stack $@opened("{{.*}}", any P) Self
// CHECK: copy_addr [[OPENED]] to [initialization] [[COPIED]] : $*@opened("{{.*}}", any P) Self
// CHECK: [[FN:%.*]] = class_method %1 : $@thick C.Type, #C.init!allocator :  (C.Type) -> (some P) -> C, $@convention(method) <τ_0_0 where τ_0_0 : P> (@in τ_0_0, @thick C.Type) -> @owned C
// CHECK: apply [[FN]]<@opened("{{.*}}", any P) Self>([[COPIED]], %1) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in τ_0_0, @thick C.Type) -> @owned C
// CHECK: dealloc_stack [[COPIED]] : $*@opened("{{.*}}", any P) Self
