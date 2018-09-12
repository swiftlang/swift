// RUN: %target-swift-frontend -emit-ir -enable-resilience -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift | %FileCheck %s

// Protocol requirements base descriptor
// CHECK: @"$S18resilient_protocol21ResilientBaseProtocolTL" = alias %swift.protocol_requirement, getelementptr (%swift.protocol_requirement, %swift.protocol_requirement* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>, <{ i32, i32, i32, i32, i32, i32, %swift.protocol_requirement }>* @"$S18resilient_protocol21ResilientBaseProtocolMp", i32 0, i32 6), i32 -1)

// Associated type requirement aliases

// CHECK: @"$S1T18resilient_protocol24ProtocolWithRequirementsPTl" = alias

