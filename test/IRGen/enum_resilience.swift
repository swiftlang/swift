// RUN: rm -rf %t && mkdir %t
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

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015enum_resilience25functionWithResilientEnum010resilient_A06MediumOAEF(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithResilientEnum(_ m: Medium) -> Medium {

// CHECK:      [[METADATA:%.*]] = call %swift.type* @_T014resilient_enum6MediumOMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 9
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* %0, %swift.opaque* %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return m
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015enum_resilience33functionWithIndirectResilientEnum010resilient_A00E8ApproachOAEF(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithIndirectResilientEnum(_ ia: IndirectApproach) -> IndirectApproach {

// CHECK:      [[METADATA:%.*]] = call %swift.type* @_T014resilient_enum16IndirectApproachOMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 9
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* %0, %swift.opaque* %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return ia
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015enum_resilience31constructResilientEnumNoPayload010resilient_A06MediumOyF
public func constructResilientEnumNoPayload() -> Medium {
// CHECK:      [[METADATA:%.*]] = call %swift.type* @_T014resilient_enum6MediumOMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 25
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* %0, i32 0, %swift.type* [[METADATA]])

// CHECK-NEXT: ret void
  return Medium.Paper
}

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015enum_resilience29constructResilientEnumPayload010resilient_A06MediumO0G7_struct4SizeVF
public func constructResilientEnumPayload(_ s: Size) -> Medium {
// CHECK:      [[METADATA:%.*]] = call %swift.type* @_T016resilient_struct4SizeVMa()
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 6
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: [[COPY:%.*]] = call %swift.opaque* %initializeWithCopy(%swift.opaque* %0, %swift.opaque* %1, %swift.type* [[METADATA]])

// CHECK-NEXT: [[METADATA2:%.*]] = call %swift.type* @_T014resilient_enum6MediumOMa()
// CHECK-NEXT: [[METADATA_ADDR2:%.*]] = bitcast %swift.type* [[METADATA2]] to i8***
// CHECK-NEXT: [[VWT_ADDR2:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR2]], [[INT:i32|i64]] -1
// CHECK-NEXT: [[VWT2:%.*]] = load i8**, i8*** [[VWT_ADDR2]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT2]], i32 25
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* %0, i32 -2, %swift.type* [[METADATA2]])

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 4
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* %1, %swift.type* [[METADATA]])

// CHECK-NEXT: ret void
  return Medium.Postcard(s)
}

// CHECK-LABEL: define{{( protected)?}} swiftcc {{i32|i64}} @_T015enum_resilience19resilientSwitchTestSi0c1_A06MediumOF(%swift.opaque* noalias nocapture)
// CHECK: [[METADATA:%.*]] = call %swift.type* @_T014resilient_enum6MediumOMa()
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 17
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FOR_SIZE:%.*]] = ptrtoint i8* [[WITNESS]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ENUM_STORAGE:%.*]] = bitcast i8* [[ALLOCA]] to %swift.opaque*

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 6
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[ENUM_COPY:%.*]] = call %swift.opaque* [[WITNESS_FN]](%swift.opaque* [[ENUM_STORAGE]], %swift.opaque* %0, %swift.type* [[METADATA]])

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 23
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK: [[TAG:%.*]] = call i32 %getEnumTag(%swift.opaque* [[ENUM_STORAGE]], %swift.type* [[METADATA]])

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

// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T015enum_resilience25resilientEnumPartialApplyySi0c1_A06MediumOcF(i8*, %swift.refcounted*)
public func resilientEnumPartialApply(_ f: (Medium) -> Int) {

// CHECK:     [[CONTEXT:%.*]] = call noalias %swift.refcounted* @swift_rt_swift_allocObject
// CHECK:     call swiftcc void @_T015enum_resilience13reabstractionyx010resilient_A06MediumOclF(i8* bitcast (void (%TSi*, %swift.opaque*, %swift.refcounted*)* @_T014resilient_enum6MediumOSiIxid_ACSiIxir_TRTA to i8*), %swift.refcounted* [[CONTEXT:%.*]], %swift.type* @_T0SiN)
  reabstraction(f)

// CHECK:     ret void
}

// CHECK-LABEL: define internal swiftcc void @_T014resilient_enum6MediumOSiIxid_ACSiIxir_TRTA(%TSi* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.refcounted* swiftself)


// Enums with resilient payloads from a different resilience domain
// require runtime metadata instantiation, just like generics.

public enum EnumWithResilientPayload {
  case OneSize(Size)
  case TwoSizes(Size, Size)
}

// Make sure we call a function to access metadata of enums with
// resilient layout.

// CHECK-LABEL: define{{( protected)?}} swiftcc %swift.type* @_T015enum_resilience20getResilientEnumTypeypXpyF()
// CHECK:      [[METADATA:%.*]] = call %swift.type* @_T015enum_resilience24EnumWithResilientPayloadOMa()
// CHECK-NEXT: ret %swift.type* [[METADATA]]

public func getResilientEnumType() -> Any.Type {
  return EnumWithResilientPayload.self
}

// Public metadata accessor for our resilient enum
// CHECK-LABEL: define{{( protected)?}} %swift.type* @_T015enum_resilience24EnumWithResilientPayloadOMa()
// CHECK: [[METADATA:%.*]] = load %swift.type*, %swift.type** @_T015enum_resilience24EnumWithResilientPayloadOML
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[METADATA]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK: cacheIsNull:
// CHECK-NEXT: call void @swift_once([[INT]]* @_T015enum_resilience24EnumWithResilientPayloadOMa.once_token, i8* bitcast (void (i8*)* @initialize_metadata_EnumWithResilientPayload to i8*))
// CHECK-NEXT: [[METADATA2:%.*]] = load %swift.type*, %swift.type** @_T015enum_resilience24EnumWithResilientPayloadOML
// CHECK-NEXT: br label %cont

// CHECK: cont:
// CHECK-NEXT: [[RESULT:%.*]] = phi %swift.type* [ [[METADATA]], %entry ], [ [[METADATA2]], %cacheIsNull ]
// CHECK-NEXT: ret %swift.type* [[RESULT]]

// Methods inside extensions of resilient enums fish out type parameters
// from metadata -- make sure we can do that
extension ResilientMultiPayloadGenericEnum {

// CHECK-LABEL: define{{( protected)?}} swiftcc %swift.type* @_T014resilient_enum32ResilientMultiPayloadGenericEnumO0B11_resilienceE16getTypeParameterxmyF(%swift.type* %"ResilientMultiPayloadGenericEnum<T>", %swift.opaque* noalias nocapture swiftself)
// CHECK: [[METADATA:%.*]] = bitcast %swift.type* %"ResilientMultiPayloadGenericEnum<T>" to %swift.type**
// CHECK-NEXT: [[T_ADDR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[METADATA]], [[INT]] 3
// CHECK-NEXT: [[T:%.*]] = load %swift.type*, %swift.type** [[T_ADDR]]
  public func getTypeParameter() -> T.Type {
    return T.self
  }
}

// CHECK-LABEL: define{{( protected)?}} private void @initialize_metadata_EnumWithResilientPayload(i8*)
