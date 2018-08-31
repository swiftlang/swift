
// RUN: %empty-directory(%t)
// RUN: %{python} %utils/chex.py < %s > %t/enum_resilience.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-ir -enable-resilience -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift | %FileCheck %t/enum_resilience.swift --check-prefix=ENUM_RES
// RUN: %target-swift-frontend -emit-ir -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift | %FileCheck %t/enum_resilience.swift --check-prefix=ENUM_NOT_RES
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -module-name enum_resilience -I %t -emit-ir -enable-resilience %s | %FileCheck %t/enum_resilience.swift -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -module-name enum_resilience -I %t -emit-ir -enable-resilience -O %s

import resilient_enum
import resilient_struct

// ENUM_RES: @"$S14resilient_enum6MediumO8PamphletyA2CcACmFWC" = {{.*}}constant i32 0
// ENUM_RES: @"$S14resilient_enum6MediumO8PostcardyAC0A7_struct4SizeVcACmFWC" = {{.*}}constant i32 1
// ENUM_RES: @"$S14resilient_enum6MediumO5PaperyA2CmFWC" = {{.*}}constant i32 2
// ENUM_RES: @"$S14resilient_enum6MediumO6CanvasyA2CmFWC" = {{.*}}constant i32 3

// ENUM_NOT_RES-NOT: @"$S14resilient_enum6MediumO8PamphletyA2CcACmFWC" =
// ENUM_NOT_RES-NOT: @"$S14resilient_enum6MediumO8PostcardyAC0A7_struct4SizeVcACmFWC" =
// ENUM_NOT_RES-NOT: @"$S14resilient_enum6MediumO5PaperyA2CmFWC" =
// ENUM_NOT_RES-NOT: @"$S14resilient_enum6MediumO6CanvasyA2CmFWC" =

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

// CHECK: @"$S15enum_resilience24EnumWithResilientPayloadOMl" =
// CHECK-SAME: internal global { %swift.type*, i8* } zeroinitializer, align

// CHECK: @"$S15enum_resilience24EnumWithResilientPayloadOMn" = {{.*}}constant
//              0x00010052
//              0x0001      - InPlaceMetadataInitialization
//              0x    0040  - IsUnique
//              0x    0012  - Enum
// CHECK-SAME: <i32 0x0001_0052>,
// CHECK-SAME: @"$S15enum_resilience24EnumWithResilientPayloadOMl"
// CHECK-SAME: @"$S15enum_resilience24EnumWithResilientPayloadOMf", i32 0, i32 1)
// CHECK-SAME: @"$S15enum_resilience24EnumWithResilientPayloadOMr"

public class Class {}

public struct Reference {
  public var n: Class
}

@_frozen public enum Either {
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

@_frozen public enum EitherFast {
  case Left(ReferenceFast)
  case Right(ReferenceFast)
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience25functionWithResilientEnumy010resilient_A06MediumOAEF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithResilientEnum(_ m: Medium) -> Medium {

// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14resilient_enum6MediumOMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// This is copying the +0 argument to be used as a return value.
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return m
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience33functionWithIndirectResilientEnumy010resilient_A00E8ApproachOAEF"(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture)
public func functionWithIndirectResilientEnum(_ ia: IndirectApproach) -> IndirectApproach {

// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14resilient_enum16IndirectApproachOMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]
// This is copying the +0 argument into the return slot.
// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void

  return ia
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience31constructResilientEnumNoPayload010resilient_A06MediumOyF"
public func constructResilientEnumNoPayload() -> Medium {
// CHECK:      [[TAG:%.*]] = load i32, i32* @"$S14resilient_enum6MediumO5PaperyA2CmFWC"
// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14resilient_enum6MediumOMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 16
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* noalias %0, i32 [[TAG]], %swift.type* [[METADATA]])

// CHECK-NEXT: ret void
  return Medium.Paper
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience29constructResilientEnumPayloady010resilient_A06MediumO0G7_struct4SizeVF"
public func constructResilientEnumPayload(_ s: Size) -> Medium {
// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK-NEXT: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK-NEXT: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK-NEXT: [[WITNESS:%.*]]  = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: [[COPY:%.*]] = call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias %0, %swift.opaque* noalias %1, %swift.type* [[METADATA]])

// CHECK-NEXT: [[TAG:%.*]] = load i32, i32* @"$S14resilient_enum6MediumO8PostcardyAC0A7_struct4SizeVcACmFWC"
// CHECK-NEXT: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14resilient_enum6MediumOMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA2:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: [[METADATA_ADDR2:%.*]] = bitcast %swift.type* [[METADATA2]] to i8***
// CHECK-NEXT: [[VWT_ADDR2:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR2]], [[INT]] -1
// CHECK-NEXT: [[VWT2:%.*]] = load i8**, i8*** [[VWT_ADDR2]]

// CHECK-NEXT: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT2]], i32 16
// CHECK-NEXT: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK-NEXT: [[WITNESS_FN:%destructiveInjectEnumTag]] = bitcast i8* [[WITNESS]]
// CHECK-NEXT: call void [[WITNESS_FN]](%swift.opaque* noalias %0, i32 [[TAG]], %swift.type* [[METADATA2]])

// CHECK-NEXT: ret void

  return Medium.Postcard(s)
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc {{i32|i64}} @"$S15enum_resilience19resilientSwitchTestySi0c1_A06MediumOF"(%swift.opaque* noalias nocapture)
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S14resilient_enum6MediumOMa"([[INT]] 0)
// CHECK: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[METADATA_ADDR:%.*]] = bitcast %swift.type* [[METADATA]] to i8***
// CHECK: [[VWT_ADDR:%.*]] = getelementptr inbounds i8**, i8*** [[METADATA_ADDR]], [[INT]] -1
// CHECK: [[VWT:%.*]] = load i8**, i8*** [[VWT_ADDR]]

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 8
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FOR_SIZE:%size]] = ptrtoint i8* [[WITNESS]]
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ALLOCA:%.*]] = alloca i8, {{.*}} [[WITNESS_FOR_SIZE]], align 16
// CHECK: [[ENUM_STORAGE:%.*]] = bitcast i8* [[ALLOCA]] to %swift.opaque*

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 2
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%initializeWithCopy]] = bitcast i8* [[WITNESS]]
// CHECK: [[ENUM_COPY:%.*]] = call %swift.opaque* [[WITNESS_FN]](%swift.opaque* noalias [[ENUM_STORAGE]], %swift.opaque* noalias %0, %swift.type* [[METADATA]])

// CHECK: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], i32 14
// CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ADDR]]
// CHECK: [[WITNESS_FN:%getEnumTag]] = bitcast i8* [[WITNESS]]
// CHECK: [[TAG:%.*]] = call i32 [[WITNESS_FN]](%swift.opaque* noalias [[ENUM_STORAGE]], %swift.type* [[METADATA]])

// CHECK:  [[PAMPHLET_CASE_TAG:%.*]] = load i32, i32* @"$S14resilient_enum6MediumO8PamphletyA2CcACmFWC"
// CHECK:  [[PAMPHLET_CASE:%.*]] = icmp eq i32 [[TAG]], [[PAMPHLET_CASE_TAG]]
// CHECK:  br i1 [[PAMPHLET_CASE]], label %[[PAMPHLET_CASE_LABEL:.*]], label %[[PAPER_CHECK:.*]]

// CHECK:  <label>:[[PAPER_CHECK]]:
// CHECK:  [[PAPER_CASE_TAG:%.*]] = load i32, i32* @"$S14resilient_enum6MediumO5PaperyA2CmFWC"
// CHECK:  [[PAPER_CASE:%.*]] = icmp eq i32 [[TAG]], [[PAPER_CASE_TAG]]
// CHECK:  br i1 [[PAPER_CASE]], label %[[PAPER_CASE_LABEL:.*]], label %[[CANVAS_CHECK:.*]]

// CHECK:  <label>:[[CANVAS_CHECK]]:
// CHECK:  [[CANVAS_CASE_TAG:%.*]] = load i32, i32* @"$S14resilient_enum6MediumO6CanvasyA2CmFWC"
// CHECK:  [[CANVAS_CASE:%.*]] = icmp eq i32 [[TAG]], [[CANVAS_CASE_TAG]]
// CHECK:  br i1 [[CANVAS_CASE]], label %[[CANVAS_CASE_LABEL:.*]], label %[[DEFAULT_CASE:.*]]

// CHECK: ; <label>:[[PAPER_CASE_LABEL]]
// CHECK: br label %[[END:.*]]

// CHECK: ; <label>:[[CANVAS_CASE_LABEL]]
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[PAMPHLET_CASE_LABEL]]
// CHECK: swift_projectBox
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[DEFAULT_CASE]]
// CHECK: br label %[[DEFAULT_CASE_DESTROY:.*]]

// CHECK: <label>:[[DEFAULT_CASE_DESTROY]]
// CHeCK: call void %destroy
// CHECK: br label %[[END]]

// CHECK: ; <label>:[[END]]
// CHECK: = phi [[INT]] [ 3, %[[DEFAULT_CASE_DESTROY]] ], [ {{.*}}, %[[PAMPHLET_CASE_LABEL]] ], [ 2, %[[CANVAS_CASE_LABEL]] ], [ 1, %[[PAPER_CASE_LABEL]] ]
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

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience25resilientEnumPartialApplyyySi0c1_A06MediumOXEF"(i8*, %swift.opaque*)
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

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.type* @"$S15enum_resilience20getResilientEnumTypeypXpyF"()
// CHECK:      [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S15enum_resilience24EnumWithResilientPayloadOMa"([[INT]] 0)
// CHECK:      [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT: ret %swift.type* [[METADATA]]

public func getResilientEnumType() -> Any.Type {
  return EnumWithResilientPayload.self
}

// Public metadata accessor for our resilient enum
// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.metadata_response @"$S15enum_resilience24EnumWithResilientPayloadOMa"(
// CHECK: [[LOAD_METADATA:%.*]] = load %swift.type*, %swift.type** getelementptr inbounds ({ %swift.type*, i8* }, { %swift.type*, i8* }* @"$S15enum_resilience24EnumWithResilientPayloadOMl", i32 0, i32 0), align
// CHECK-NEXT: [[COND:%.*]] = icmp eq %swift.type* [[LOAD_METADATA]], null
// CHECK-NEXT: br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK: cacheIsNull:
// CHECK-NEXT: [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S15enum_resilience24EnumWithResilientPayloadOMn" to %swift.type_descriptor*))
// CHECK-NEXT: [[RESPONSE_METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT: [[RESPONSE_STATE:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 1
// CHECK-NEXT: br label %cont

// CHECK: cont:
// CHECK-NEXT: [[RESULT_METADATA:%.*]] = phi %swift.type* [ [[LOAD_METADATA]], %entry ], [ [[RESPONSE_METADATA]], %cacheIsNull ]
// CHECK-NEXT: [[RESULT_STATE:%.*]] = phi [[INT]] [ 0, %entry ], [ [[RESPONSE_STATE]], %cacheIsNull ]
// CHECK-NEXT: [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[RESULT_METADATA]], 0
// CHECK-NEXT: [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[RESULT_STATE]], 1
// CHECK-NEXT: ret %swift.metadata_response [[T1]]

// Methods inside extensions of resilient enums fish out type parameters
// from metadata -- make sure we can do that
extension ResilientMultiPayloadGenericEnum {

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.type* @"$S14resilient_enum32ResilientMultiPayloadGenericEnumO0B11_resilienceE16getTypeParameterxmyF"(%swift.type* %"ResilientMultiPayloadGenericEnum<T>", %swift.opaque* noalias nocapture swiftself)
// CHECK: [[METADATA:%.*]] = bitcast %swift.type* %"ResilientMultiPayloadGenericEnum<T>" to %swift.type**
// CHECK-NEXT: [[T_ADDR:%.*]] = getelementptr inbounds %swift.type*, %swift.type** [[METADATA]], [[INT]] 2
// CHECK-NEXT: [[T:%.*]] = load %swift.type*, %swift.type** [[T_ADDR]]
  public func getTypeParameter() -> T.Type {
    return T.self
  }
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S15enum_resilience39constructExhaustiveWithResilientMembers010resilient_A011SimpleShapeOyF"(%T14resilient_enum11SimpleShapeO* noalias nocapture sret)
// CHECK: [[BUFFER:%.*]] = bitcast %T14resilient_enum11SimpleShapeO* %0 to %swift.opaque*
// CHECK: [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 0)
// CHECK-NEXT: [[METADATA:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK: [[STORE_TAG:%.*]] = bitcast i8* {{%.+}} to void (%swift.opaque*, i32, i32, %swift.type*)* 
// CHECK-NEXT: call void [[STORE_TAG]](%swift.opaque* noalias [[BUFFER]], i32 1, i32 1, %swift.type* [[METADATA]])
// CHECK-NEXT: ret void
// CHECK-NEXT: {{^}$}}
public func constructExhaustiveWithResilientMembers() -> SimpleShape {
  return .KleinBottle
}

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc { i{{64|32}}, i8 } @"$S15enum_resilience19constructFullyFixed010resilient_A00dE6LayoutOyF"()
// CHECK: ret { [[INT]], i8 } { [[INT]] 0, i8 1 }
// CHECK-NEXT: {{^}$}}
public func constructFullyFixed() -> FullyFixedLayout {
  return .noPayload
}

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S15enum_resilience24EnumWithResilientPayloadOMr"(%swift.type*, i8*, i8**)
// CHECK:        [[TUPLE_LAYOUT:%.*]] = alloca %swift.full_type_layout
// CHECK:        [[SIZE_RESPONSE:%.*]] = call swiftcc %swift.metadata_response @"$S16resilient_struct4SizeVMa"([[INT]] 319)
// CHECK-NEXT:   [[SIZE_METADATA:%.*]] = extractvalue %swift.metadata_response [[SIZE_RESPONSE]], 0
// CHECK-NEXT:   [[SIZE_STATE:%.*]] = extractvalue %swift.metadata_response [[SIZE_RESPONSE]], 1
// CHECK-NEXT:   [[T0:%.*]] = icmp ule [[INT]] [[SIZE_STATE]], 63
// CHECK-NEXT:   br i1 [[T0]], label %[[SATISFIED1:.*]], label
// CHECK:      [[SATISFIED1]]:
// CHECK-NEXT:   [[T0:%.*]] = bitcast %swift.type* [[SIZE_METADATA]] to i8***
// CHECK-NEXT:   [[T1:%.*]] = getelementptr inbounds i8**, i8*** [[T0]], [[INT]] -1
// CHECK-NEXT:   [[SIZE_VWT:%.*]] = load i8**, i8*** [[T1]],
// CHECK-NEXT:   [[SIZE_LAYOUT_1:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK-NEXT:   store i8** [[SIZE_LAYOUT_1]],
// CHECK-NEXT:   getelementptr
// CHECK-NEXT:   [[SIZE_LAYOUT_2:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK-NEXT:   [[SIZE_LAYOUT_3:%.*]] = getelementptr inbounds i8*, i8** [[SIZE_VWT]], i32 8
// CHECK-NEXT:   call swiftcc [[INT]] @swift_getTupleTypeLayout2(%swift.full_type_layout* [[TUPLE_LAYOUT]], i8** [[SIZE_LAYOUT_2]], i8** [[SIZE_LAYOUT_3]])
// CHECK-NEXT:   [[T0:%.*]] = bitcast %swift.full_type_layout* [[TUPLE_LAYOUT]] to i8**
// CHECK-NEXT:   store i8** [[T0]],
// CHECK:        call void @swift_initEnumMetadataMultiPayload
// CHECK:        phi %swift.type* [ [[SIZE_METADATA]], %entry ], [ null, %[[SATISFIED1]] ]
// CHECK:        phi [[INT]] [ 63, %entry ], [ 0, %[[SATISFIED1]] ]


public protocol Prot {
}

private enum ProtGenEnumWithSize<T: Prot> {
    case c1(s1: Size)
    case c2(s2: Size)
}

// CHECK-LABEL: define linkonce_odr hidden %T15enum_resilience19ProtGenEnumWithSize33_59077B69D65A4A3BEE0C93708067D5F0LLO* @"$S15enum_resilience19ProtGenEnumWithSize33_59077B69D65A4A3BEE0C93708067D5F0LLOyxGAA0C0RzlWOh"(%T15enum_resilience19ProtGenEnumWithSize
// CHECK:   ret %T15enum_resilience19ProtGenEnumWithSize33_59077B69D65A4A3BEE0C93708067D5F0LLO* %0
