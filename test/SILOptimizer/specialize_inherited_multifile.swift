// RUN: %target-swift-frontend -primary-file %s %S/Inputs/specialize_inherited_multifile.swift -O -emit-sil -sil-verify-all | %FileCheck %s

@_semantics("optimize.sil.never") func takesBase<T : Base>(t: T) {}

@inline(never) func takesHasAssocType<T : HasAssocType>(t: T) {
  takesBase(t: t.value)
}

// Make sure the ConcreteDerived : Base conformance is available here.

// CHECK-LABEL: sil shared [noinline] @_TTSg5C30specialize_inherited_multifile20ConcreteHasAssocTypeS0_S_12HasAssocTypeS____TF30specialize_inherited_multifile17takesHasAssocTypeuRxS_12HasAssocTyperFT1tx_T_ : $@convention(thin) (@owned ConcreteHasAssocType) -> ()
// CHECK: [[FN:%.*]] = function_ref @_TF30specialize_inherited_multifile9takesBaseuRxS_4BaserFT1tx_T_
// CHECK: apply [[FN]]<ConcreteDerived>({{%.*}})
// CHECK: return

public func takesConcreteHasAssocType(c: ConcreteHasAssocType) {
  takesHasAssocType(t: c)
}
