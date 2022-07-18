// RUN: %target-swift-frontend -emit-sil -O -primary-file %s | %FileCheck %s

// FIXME: Ideally, this should be a sil-opt test, but TypeRepr cannot model
// types like '(@opened("...") P).Assoc' as of now.

protocol P {
  associatedtype Assoc: P

  func f() -> Self.Assoc
}

// CHECK-LABEL: sil hidden [always_inline] @$s35mandatory_inlining_open_existential6callee1pAA1P_pAaD_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access {{%[0-9]+}} : $*P to $*[[OPENED_TY:@opened\("[-A-F0-9]+"\) P]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.f : <Self where Self : P> (Self) -> () -> Self.Assoc
// CHECK: [[RESULT:%[0-9]+]] = init_existential_addr {{%[0-9]+}} : $*P, $([[OPENED_TY]]).Assoc
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[RESULT]], [[OPENED]])
// CHECK: }
@inline(__always)
func callee(p: any P) -> any P {
  return p.f()
}

// CHECK-LABEL: sil hidden @$s35mandatory_inlining_open_existential6caller1pAA1P_pAaD_p_tF
// CHECK: [[OPENED:%[0-9]+]] = open_existential_addr immutable_access {{%[0-9]+}} : $*P to $*[[OPENED_TY:@opened\("[-A-F0-9]+"\) P]]
// CHECK: [[WITNESS:%[0-9]+]] = witness_method $[[OPENED_TY]], #P.f : <Self where Self : P> (Self) -> () -> Self.Assoc
// CHECK: [[RESULT:%[0-9]+]] = init_existential_addr {{%[0-9]+}} : $*P, $([[OPENED_TY]]).Assoc
// CHECK: apply [[WITNESS]]<[[OPENED_TY]]>([[RESULT]], [[OPENED]])
// CHECK: }
func caller(p: any P) -> any P {
  return callee(p: p)
}
