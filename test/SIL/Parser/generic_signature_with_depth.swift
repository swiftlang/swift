
// RUN: %target-swift-frontend -module-name generic_signature_with_depth %s -emit-silgen | %target-sil-opt | %FileCheck %s

protocol mmGeneratorType {
  associatedtype Element
}

protocol mmSequenceType {
  associatedtype Generator : mmGeneratorType
}

protocol mmCollectionType : mmSequenceType {
}

protocol mmExt : mmCollectionType {
 mutating func extend<
     S : mmSequenceType
 > (_ seq: S) where S.Generator.Element == Self.Generator.Element
}

// CHECK-LABEL:  @$S28generic_signature_with_depth4testyxx_q_tAA5mmExtRzAaCR_9Generator_7ElementQY_AD_AERTzr0_lF : $@convention(thin) <EC1, EC2 where EC1 : mmExt, EC2 : mmExt, EC1.Generator.Element == EC2.Generator.Element> (@in_guaranteed EC1, @in_guaranteed EC2) -> @out EC1 {
// CHECK: witness_method $EC1, #mmExt.extend!1 : {{.*}} : $@convention(witness_method: mmExt) <τ_0_0 where τ_0_0 : mmExt><τ_1_0 where τ_1_0 : mmSequenceType, τ_0_0.Generator.Element == τ_1_0.Generator.Element> (@in_guaranteed τ_1_0, @inout τ_0_0) -> ()
// CHECK: apply {{%[0-9]+}}<EC1, EC2>({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(witness_method: mmExt) <τ_0_0 where τ_0_0 : mmExt><τ_1_0 where τ_1_0 : mmSequenceType, τ_0_0.Generator.Element == τ_1_0.Generator.Element> (@in_guaranteed τ_1_0, @inout τ_0_0) -> ()

func test<
   EC1 : mmExt,
   EC2 : mmExt
>
(_ lhs: EC1, _ rhs: EC2) -> EC1 where EC1.Generator.Element == EC2.Generator.Element {
 var lhs = lhs
 lhs.extend(rhs)
 return lhs
}
