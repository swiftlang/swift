// RUN: %target-swift-frontend -emit-ir -enable-resilience -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift | %FileCheck %s

// Associated type requirement aliases

// CHECK: @"$S1T18resilient_protocol24ProtocolWithRequirementsPTl" = alias

