// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test -experimental-lazy-typecheck | %FileCheck %s

public protocol Proto {
  func requirement()
}

public protocol ProtoWithAssociatedType {
  associatedtype A
  func requirement() -> A
  func otherRequirement(_ a: A)
}

public struct StructConformingToProto: Proto {
  // CHECK: sil{{.*}} @$s4Test23StructConformingToProtoV11requirementyyF : $@convention(method) (StructConformingToProto) -> () {
  public func requirement() {}
}

// CHECK: sil shared [transparent] [serialized] [thunk]{{.*}} @$s4Test23StructConformingToProtoVAA0E0A2aDP11requirementyyFTW : $@convention(witness_method: Proto) (@in_guaranteed StructConformingToProto) -> () {

public struct StructConformingToProtoInExtension {}

extension StructConformingToProtoInExtension: Proto {
  // CHECK: sil{{.*}} @$s4Test34StructConformingToProtoInExtensionV11requirementyyF : $@convention(method) (StructConformingToProtoInExtension) -> () {
  public func requirement() {}
}
// CHECK: sil shared [transparent] [serialized] [thunk]{{.*}} @$s4Test34StructConformingToProtoInExtensionVAA0E0A2aDP11requirementyyFTW : $@convention(witness_method: Proto) (@in_guaranteed StructConformingToProtoInExtension) -> () {

public struct StructConformingToProtoWithAssociatedType: ProtoWithAssociatedType {
  // CHECK: sil{{.*}} @$s4Test41StructConformingToProtoWithAssociatedTypeV11requirementSiyF : $@convention(method) (StructConformingToProtoWithAssociatedType) -> Int {
  public func requirement() -> Int { return 1 }

  // CHECK: sil{{.*}} @$s4Test41StructConformingToProtoWithAssociatedTypeV16otherRequirementyySiF : $@convention(method) (Int, StructConformingToProtoWithAssociatedType) -> () {
  public func otherRequirement(_ a: A) {}
}
// CHECK: sil shared [transparent] [serialized] [thunk]{{.*}} @$s4Test41StructConformingToProtoWithAssociatedTypeVAA0efgH0A2aDP11requirement1AQzyFTW : $@convention(witness_method: ProtoWithAssociatedType) (@in_guaranteed StructConformingToProtoWithAssociatedType) -> @out Int {
// CHECK: sil shared [transparent] [serialized] [thunk]{{.*}} @$s4Test41StructConformingToProtoWithAssociatedTypeVAA0efgH0A2aDP16otherRequirementyy1AQzFTW : $@convention(witness_method: ProtoWithAssociatedType) (@in_guaranteed Int, @in_guaranteed StructConformingToProtoWithAssociatedType) -> () {

// CHECK-LABEL: sil_witness_table [serialized] StructConformingToProto: Proto module Test {
// CHECK-NEXT:    method #Proto.requirement: <Self where Self : Proto> (Self) -> () -> () : @$s4Test23StructConformingToProtoVAA0E0A2aDP11requirementyyFTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table [serialized] StructConformingToProtoInExtension: Proto module Test {
// CHECK-NEXT:    method #Proto.requirement: <Self where Self : Proto> (Self) -> () -> () : @$s4Test34StructConformingToProtoInExtensionVAA0E0A2aDP11requirementyyFTW
// CHECK-NEXT:  }

// CHECK-LABEL: sil_witness_table [serialized] StructConformingToProtoWithAssociatedType: ProtoWithAssociatedType module Test {
// CHECK-NEXT:    associated_type A: Int
// CHECK-NEXT:    method #ProtoWithAssociatedType.requirement: <Self where Self : ProtoWithAssociatedType> (Self) -> () -> Self.A : @$s4Test41StructConformingToProtoWithAssociatedTypeVAA0efgH0A2aDP11requirement1AQzyFTW
// CHECK-NEXT:    method #ProtoWithAssociatedType.otherRequirement: <Self where Self : ProtoWithAssociatedType> (Self) -> (Self.A) -> () : @$s4Test41StructConformingToProtoWithAssociatedTypeVAA0efgH0A2aDP16otherRequirementyy1AQzFTW
// CHECK-NEXT:  }
