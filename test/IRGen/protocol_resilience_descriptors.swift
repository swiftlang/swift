// RUN: %empty-directory(%t)

// Resilient protocol definition
// RUN: %target-swift-frontend -emit-ir -enable-resilience -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift | %FileCheck -check-prefix=CHECK-DEFINITION %s

// Resilient protocol usage
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift

// RUN: %target-swift-frontend -I %t -emit-ir -enable-resilience -assume-parsing-unqualified-ownership-sil %s | %FileCheck %s -DINT=i%target-ptrsize -check-prefix=CHECK-USAGE

// ----------------------------------------------------------------------------
// Resilient protocol definition
// ----------------------------------------------------------------------------

// Protocol requirements base descriptor
// CHECK-DEFINITION: @"$S18resilient_protocol21ResilientBaseProtocolTL" = alias %swift.protocol_requirement, getelementptr (%swift.protocol_requirement, %swift.protocol_requirement* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>, <{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>* @"$S18resilient_protocol21ResilientBaseProtocolMp", i32 0, i32 6), i32 -1)

// Associated type requirement aliases

// CHECK-DEFINITION: @"$S1T18resilient_protocol24ProtocolWithRequirementsPTl" = alias

// ----------------------------------------------------------------------------
// Resilient protocol usage
// ----------------------------------------------------------------------------
import resilient_protocol

// CHECK-USAGE: define{{( dllexport)?}}{{( protected)?}} swiftcc %swift.type* @"$S31protocol_resilience_descriptors17assocTypeMetadatay1TQzmxm010resilient_A024ProtocolWithRequirementsRzlF"(%swift.type*, %swift.type* [[PWD:%.*]], i8** [[WTABLE:%.*]])
public func assocTypeMetadata<PWR: ProtocolWithRequirements>(_: PWR.Type) -> PWR.T.Type {
  // CHECK-USAGE: [[WITNESS_ADDR:%.*]] = getelementptr inbounds i8*, i8** %PWR.ProtocolWithRequirements, [[INT]] udiv ([[INT]] sub ([[INT]] ptrtoint (%swift.protocol_requirement* @"$S1T18resilient_protocol24ProtocolWithRequirementsPTl" to [[INT]]), [[INT]] ptrtoint (%swift.protocol_requirement* @"$S18resilient_protocol24ProtocolWithRequirementsTL" to [[INT]])), [[INT]] {{(8|16)}})
  // CHECK-USAGE: load i8*, i8** [[WITNESS_ADDR]], align 8
  return PWR.T.self
}
