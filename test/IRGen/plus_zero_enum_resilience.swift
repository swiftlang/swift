// REQUIRES: plus_zero_runtime

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience %s | %FileCheck %s
// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -O %s

import resilient_enum
import resilient_struct

// CHECK: %swift.type = type { [[INT:i32|i64]] }

// CHECK: %T15enum_resilience5ClassC = type <{ %swift.refcounted }>
// CHECK: %T15enum_resilience9ReferenceV = type <{ %T15enum_resilience5ClassC* }>

// Public fixed layout struct contains a public resilient struct,
// cannot use spare bits

// CHECK: %T15enum_resilience6EitherO = type <{ [[REFERENCE_TYPE:\[(4|8) x i8\]]], [1 x i8] }>

// Public resilient struct contains a public resilient struct,
// can use spare bits

// CHECK: %T15enum_resilience15ResilientEitherO = type <{ [[REFERENCE_TYPE]] }>

// Internal fixed layout struct contains a public resilient struct,
// can use spare bits

// CHECK: %T15enum_resilience14InternalEitherO = type <{ [[REFERENCE_TYPE]] }>

// Public fixed layout struct contains a fixed layout struct,
// can use spare bits

// CHECK: %T15enum_resilience10EitherFastO = type <{ [[REFERENCE_TYPE]] }>

public class Class {}

public struct Reference {
  public var n: Class
}

@_fixed_layout public enum Either {
  case Left(Reference)
  case Right(Reference)
}

public enum ResilientEither {
  case Left(Reference)
  case Right(Reference)
}

enum InternalEither {
  case Left(Reference)
  case Right(Reference)
}

@_fixed_layout public struct ReferenceFast {
  public var n: Class
}

@_fixed_layout public enum EitherFast {
  case Left(ReferenceFast)
  case Right(ReferenceFast)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S15enum_resilience25functionWithResilientEnumy010resilient_A06MediumOAEF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithResilientEnum(_ m: Medium) -> Medium {

// CHECK:      [[METADATA:%.*]] = call %swift.type* @"$S14resilient_enum6MediumOMa"()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// This is copying the +0 argument to be used as a return value.
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return m
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S15enum_resilience33functionWithIndirectResilientEnumy010resilient_A00E8ApproachOAEF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithIndirectResilientEnum(_ ia: IndirectApproach) -> IndirectApproach {

// CHECK:      [[METADATA:%.*]] = call %swift.type* @"$S14resilient_enum16IndirectApproachOMa"()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// This is copying the +0 argument into the return slot.
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return ia
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S15enum_resilience31constructResilientEnumNoPayload010resilient_A06MediumOyF"
public func constructResilientEnumNoPayload() -> Medium {
// CHECK:      [[METADATA:%.*]] = call %swift.type* @"$S14resilient_enum6MediumOMa"()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 17
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* noalias %0, i32 0, %swift.type* [[METADATA]])

// CHECK-NEXT: ret void
  return Medium.Paper
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S15enum_resilience29constructResilientEnumPayloady010resilient_A06MediumO0G7_struct4SizeVF"
public func constructResilientEnumPayload(_ s: Size) -> Medium {
// CHECK:      [[METADATA:%.*]] = call %swift.type* @"$S16resilient_struct4SizeVMa"()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: [[COPY:%.*]] = call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])

// CHECK-NEXT: [[METADATA2:%.*]] = call %swift.type* @"$S14resilient_enum6MediumOMa"()
// CHECK-NEXT: [[METADATA_ADDR2:%.*]] = bitcast %swift.type* [[METADATA2]] to i8***
// CHECK-NEXT: [[VWT_ADDR2:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR2]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT2:%.*]] = load i8**, i8*** [[VWT_ADDR2]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT2]], i32 17
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%destructiveInjectEnumTag]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* noalias %0, i32 -2, %swift.type* [[METADATA2]])
// CHECK-NEXT: ret void

  return Medium.Postcard(s)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc {{i32|i64}} @"$S15enum_resilience19resilientSwitchTestySi0c1_A06MediumOF"(%swift.opaque* noalias nocapture)
// CHECK: [[METADATA:%.*]] = call %swift.type* @"$S14resilient_enum6MediumOMa"()
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 9
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FOR_SIZE:%size]] = ptrtoint i8* [[WITNESS]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ENUM_STORAGE:%.*]] = bitcast i8* [[ALLOCA]] to %swift.opaque*

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK: [[ENUM_COPY:%.*]] = call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias [[ENUM_STORAGE]], %swift.opaque* noalias %0, %swift.type* [[METADATA]])

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 15
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%getEnumTag]] = bitcast i8* [[WITNESS]]
// CHECK: [[TAG:%.*]] = call i32 [[WITNESS_FN]](%swift.opaque* noalias [[ENUM_STORAGE]], %swift.type* [[METADATA]])

// CHECK: switch i32 [[TAG]], label %[[DEFAULT_CASE:.*]] [
// CHECK:   i32 -1, label %[[PAMPHLET_CASE:.*]]
// CHECK:   i32 0, label %[[PAPER_CASE:.*]]
// CHECK:   i32 1, label %[[CANVAS_CASE:.*]]
// CHECK: ]

// CHECK: ; <label>:[[PAPER_CASE]]
// CHECK: br label %[[END:.*]]

// CHECK: ; <label>:[[CANVAS_CASE]]
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[PAMPHLET_CASE]]
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[DEFAULT_CASE]]
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[END]]
// CHECK: ret

public func resilientSwitchTest(_ m: Medium) -> Int {
  switch m {
  case .Paper:
    return 1
  case .Canvas:
    return 2
  case .Pamphlet(let m):
    return resilientSwitchTest(m)
  default:
    return 3
  }
}

public func reabstraction<T>(_ f: (Medium) -> T) {}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S15enum_resilience25resilientEnumPartialApplyyySi0c1_A06MediumOXEF"(i8*, %swift.opaque*)
public func resilientEnumPartialApply(_ f: (Medium) -> Int) {

// CHECK:     [[CONTEXT:%.*]] = call noalias %swift.refcounted* @swift_allocObject
// CHECK:     call swiftcc void @"$S15enum_resilience13reabstractionyyx010resilient_A06MediumOXElF"(i8* bitcast (void (%TSi*, %swift.opaque*, %swift.refcounted*)* @"$S14resilient_enum6MediumOSiIgnd_ACSiIegnr_TRTA" to i8*), %swift.opaque* [[CONTEXT:%.*]], %swift.type* @"$SSiN")
  reabstraction(f)

// CHECK:     ret void
}

// CHECK-LABEL: define internal swiftcc void @"$S14resilient_enum6MediumOSiIgnd_ACSiIegnr_TRTA"(%TSi* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.refcounted* swiftself)


// Enums with resilient payloads from a different resilience domain
// require runtime metadata instantiation, just like generics.

public enum EnumWithResilientPayload {
  case OneSize(Size)
  case TwoSizes(Size, Size)
}

// Make sure we call a function to access metadata of enums with
// resilient layout.

// CHECK-LABEL: define{{( protected)?}} swiftcc %swift.type* @"$S15enum_resilience20getResilientEnumTypeypXpyF"()
// CHECK:      [[METADATA:%.*]] = call %swift.type* @"$S15enum_resilience24EnumWithResilientPayloadOMa"()
// CHECK-NEXT: ret %swift.type* [[METADATA]]

public func getResilientEnumType() -> Any.Type {
  return EnumWithResilientPayload.self
}

// Public metadata accessor for our resilient enum
// CHECK-LABEL: define{{( protected)?}} %swift.type* @"$S15enum_resilience24EnumWithResilientPayloadOMa"()
// CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** @"$S15enum_resilience24EnumWithResilientPayloadOML"
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[METADATA]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK: cacheIsNull:
// CHECK-NEXT: call void @swift_once([[INT]]* @"$S15enum_resilience24EnumWithResilientPayloadOMa.once_token", i8* bitcast (void (i8*)* @initialize_metadata_EnumWithResilientPayload to i8*), i8* undef)
// CHECK-NEXT: [[METADATA2:%.*]] = load %swift.type*, %swift.type** @"$S15enum_resilience24EnumWithResilientPayloadOML"
// CHECK-NEXT: br label %cont

// CHECK: cont:
// CHECK-NEXT: [[RESULT:%.*]] = phi %swift.type* [ [[METADATA]], %entry ], [ [[METADATA2]], %cacheIsNull ]
// CHECK-NEXT: ret %swift.type* [[RESULT]]

// Methods inside extensions of resilient enums fish out type parameters
// from metadata -- make sure we can do that
extension ResilientMultiPayloadGenericEnum {

// CHECK-LABEL: define{{( protected)?}} swiftcc %swift.type* @"$S14resilient_enum32ResilientMultiPayloadGenericEnumO0B11_resilienceE16getTypeParameterxmyF"(%swift.type* %"ResilientMultiPayloadGenericEnum<T>", %swift.opaque* noalias nocapture swiftself)
// CHECK: [[METADATA:%.*]] = bitcast %swift.type* %"ResilientMultiPayloadGenericEnum<T>" to %swift.type**
// CHECK-NEXT: [[T_ADDR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[METADATA]], [[INT]] 2
// CHECK-NEXT: [[T:%.*]] = load %swift.type*, %swift.type** [[T_ADDR]]
  public func getTypeParameter() -> T.Type {
    return T.self
  }
}

// CHECK-LABEL: define{{( protected)?}} private void @initialize_metadata_EnumWithResilientPayload(i8*)
// CHECK: call void @swift_initEnumMetadataMultiPayload(%swift.type* {{.*}}, [[INT]] 256, [[INT]] 2, i8*** {{.*}})



public protocol Prot {
}

private enum ProtGenEnumWithSize<T: Prot> {
    case c1(s1: Size)
    case c2(s2: Size)
}

// CHECK-LABEL: define{{( protected)?}} internal %T15enum_resilience19ProtGenEnumWithSize33_59077B69D65A4A3BEE0C93708067D5F0LLO* @"$S15enum_resilienceytWh2_"(%T15enum_resilience19ProtGenEnumWithSize
// CHECK:   ret %T15enum_resilience19ProtGenEnumWithSize33_59077B69D65A4A3BEE0C93708067D5F0LLO* %0
