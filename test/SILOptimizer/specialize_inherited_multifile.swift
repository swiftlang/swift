
// RUN: %target-swift-frontend -module-name specialize_inherited_multifile -primary-file %s %S/Inputs/specialize_inherited_multifile.swift -O -emit-sil -sil-verify-all | %FileCheck %s

@_optimize(none) func takesBase<T : Base>(t: T) {}

@inline(never) func takesHasAssocType<T : HasAssocType>(t: T) {
  takesBase(t: t.value)
}

// Make sure the ConcreteDerived : Base conformance is available here.

// CHECK-LABEL: sil shared [noinline] @$s30specialize_inherited_multifile17takesHasAssocType1tyx_tAA0efG0RzlFAA08ConcreteefG0C_Tg5 : $@convention(thin) (@guaranteed ConcreteHasAssocType) -> ()
// CHECK: [[FN:%.*]] = function_ref @$s30specialize_inherited_multifile9takesBase1tyx_tAA0E0RzlF
// CHECK: apply [[FN]]<ConcreteDerived>({{%.*}})
// CHECK: return

public func takesConcreteHasAssocType(c: ConcreteHasAssocType) {
  takesHasAssocType(t: c)
}
