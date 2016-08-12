// RUN: %target-swift-frontend %s -emit-silgen | %target-sil-opt | %FileCheck %s

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
     where S.Generator.Element == Self.Generator.Element
 > (_ seq: S)
}

// CHECK-LABEL:  @_TF28generic_signature_with_depth4testu0_RxS_5mmExt_S0_Wx9Generator7Element_zW_S1_S2__rFTxq__x : $@convention(thin) <EC1, EC2 where EC1 : mmExt, EC2 : mmExt, EC1.Generator : mmGeneratorType, EC2.Generator : mmGeneratorType, EC1.Generator.Element == EC2.Generator.Element> (@in EC1, @in EC2) -> @out EC1 {
// CHECK: witness_method $EC1, #mmExt.extend!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : mmExt, τ_0_0.Generator : mmGeneratorType><τ_1_0 where τ_1_0 : mmSequenceType, τ_1_0.Generator : mmGeneratorType, τ_1_0.Generator.Element == τ_0_0.Generator.Element> (@in τ_1_0, @inout τ_0_0) -> ()
// CHECK: apply {{%[0-9]+}}<EC1, EC1.Generator, EC1.Generator.Element, EC2, EC2.Generator>({{%[0-9]+}}, {{%[0-9]+}}) : $@convention(witness_method) <τ_0_0 where τ_0_0 : mmExt, τ_0_0.Generator : mmGeneratorType><τ_1_0 where τ_1_0 : mmSequenceType, τ_1_0.Generator : mmGeneratorType, τ_1_0.Generator.Element == τ_0_0.Generator.Element> (@in τ_1_0, @inout τ_0_0) -> ()

func test<
   EC1 : mmExt,
   EC2 : mmExt
   where EC1.Generator.Element == EC2.Generator.Element
>
(_ lhs: EC1, _ rhs: EC2) -> EC1 {
 var lhs = lhs
 lhs.extend(rhs)
 return lhs
}
