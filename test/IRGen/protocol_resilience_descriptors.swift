// RUN: %empty-directory(%t)

// Resilient protocol definition
// RUN: %target-swift-frontend -emit-ir -enable-resilience -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift | %FileCheck -DINT=i%target-ptrsize -check-prefix=CHECK-DEFINITION %s

// Resilient protocol usage
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift

// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -assume-parsing-unqualified-ownership-sil %s | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE

// ----------------------------------------------------------------------------
// Resilient protocol definition
// ----------------------------------------------------------------------------

// CHECK: @"default assoc type x" = linkonce_odr hidden constant
// CHECK-SAME: i8 -1, [1 x i8] c"x", i8 0

// CHECK: @"default assoc type \01____y2T118resilient_protocol29ProtocolWithAssocTypeDefaultsPQzG 18resilient_protocol7WrapperV" =

// Protocol descriptor
// CHECK-DEFINITION-LABEL: @"$s18resilient_protocol29ProtocolWithAssocTypeDefaultsMp" ={{( protected)?}} constant
// CHECK-DEFINITION-SAME: @"$s18resilient_protocol29ProtocolWithAssocTypeDefaultsP2T2AC_AA014OtherResilientC0TN"

// Associated type default + flags
// CHECK-DEFINITION-SAME: [[INT]] add
// CHECK-DEFINITION-SAME: @"default assoc type _____y2T1_____QzG 18resilient_protocol7WrapperV AA29ProtocolWithAssocTypeDefaultsP"
// CHECK-DEFINITION-SAME: [[INT]] 1

// Protocol requirements base descriptor
// CHECK-DEFINITION: @"$s18resilient_protocol21ResilientBaseProtocolTL" ={{( dllexport)?}}{{( protected)?}} alias %swift.protocol_requirement, getelementptr (%swift.protocol_requirement, %swift.protocol_requirement* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>, <{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>* @"$s18resilient_protocol21ResilientBaseProtocolMp", i32 0, i32 6), i32 -1)

// Associated type and conformance

// CHECK-DEFINITION: @"$s1T18resilient_protocol24ProtocolWithRequirementsPTl" ={{( dllexport)?}}{{( protected)?}} alias
// CHECK-DEFINITION: @"$s18resilient_protocol29ProtocolWithAssocTypeDefaultsP2T2AC_AA014OtherResilientC0Tn" ={{( dllexport)?}}{{( protected)?}} alias

// Default associated conformance witnesses
// CHECK-DEFINITION-LABEL: define internal swiftcc i8** @"$s18resilient_protocol29ProtocolWithAssocTypeDefaultsP2T2AC_AA014OtherResilientC0TN"

import resilient_protocol

// ----------------------------------------------------------------------------
// Resilient witness tables
// ----------------------------------------------------------------------------
// CHECK-USAGE-LABEL: $s31protocol_resilience_descriptors34ConformsToProtocolWithRequirementsVyxG010resilient_A00fgH0AAMc" =
// CHECK-USAGE-SAME: {{got.|__imp_}}$s1T18resilient_protocol24ProtocolWithRequirementsPTl
// CHECK-USAGE-SAME: @"symbolic x"
public struct ConformsToProtocolWithRequirements<Element>
    : ProtocolWithRequirements {
  public typealias T = Element
  public func first() { }
  public func second() { }
}

public protocol P { }
public struct ConditionallyConforms<Element> { }
public struct Y { }

// CHECK-USAGE-LABEL: @"$s31protocol_resilience_descriptors1YV010resilient_A022OtherResilientProtocolAAMc" =
// CHECK-USAGE-SAME: i32 131072,
// CHECK-USAGE-SAME: i16 1,
// CHECK-USAGE-SAME: i16 0
extension Y: OtherResilientProtocol { }

// CHECK-USAGE: @"$s31protocol_resilience_descriptors29ConformsWithAssocRequirementsV010resilient_A008ProtocoleF12TypeDefaultsAAMc" =
// CHECK-USAGE-SAME: $s18resilient_protocol29ProtocolWithAssocTypeDefaultsP2T2AC_AA014OtherResilientC0Tn
// CHECK-USAGE-SAME: $s31protocol_resilience_descriptors29ConformsWithAssocRequirementsV010resilient_A008ProtocoleF12TypeDefaultsAA2T2AdEP_AD014OtherResilientI0PWT
public struct ConformsWithAssocRequirements : ProtocolWithAssocTypeDefaults {
}

// CHECK-USAGE: @"$s31protocol_resilience_descriptors21ConditionallyConformsVyxG010resilient_A024ProtocolWithRequirementsAaeFRzAA1YV1TRtzlMc"
extension ConditionallyConforms: ProtocolWithRequirements
where Element: ProtocolWithRequirements, Element.T == Y {
  public typealias T = Element.T
  public func first() { }
  public func second() { }
}

// ----------------------------------------------------------------------------
// Resilient protocol usage
// ----------------------------------------------------------------------------

// CHECK-USAGE: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.type* @"$s31protocol_resilience_descriptors17assocTypeMetadatay1TQzmxm010resilient_A024ProtocolWithRequirementsRzlF"(%swift.type*, %swift.type* [[PWD:%.*]], i8** [[WTABLE:%.*]])
public func assocTypeMetadata<PWR: ProtocolWithRequirements>(_: PWR.Type) -> PWR.T.Type {
  // CHECK-USAGE: call swiftcc %swift.metadata_response @swift_getAssociatedTypeWitness([[INT]] 0, i8** %PWR.ProtocolWithRequirements, %swift.type* %PWR, %swift.protocol_requirement* @"$s18resilient_protocol24ProtocolWithRequirementsTL", %swift.protocol_requirement* @"$s1T18resilient_protocol24ProtocolWithRequirementsPTl")
  return PWR.T.self
}

func useOtherResilientProtocol<T: OtherResilientProtocol>(_: T.Type) { }

// CHECK-USAGE: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s31protocol_resilience_descriptors23extractAssocConformanceyyx010resilient_A0012ProtocolWithE12TypeDefaultsRzlF"
public func extractAssocConformance<T: ProtocolWithAssocTypeDefaults>(_: T) {
  // CHECK-USAGE: swift_getAssociatedConformanceWitness
  useOtherResilientProtocol(T.T2.self)
}
