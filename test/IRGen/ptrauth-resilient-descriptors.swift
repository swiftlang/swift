// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-ir -enable-library-evolution %s | %FileCheck %s

// REQUIRES: CPU=arm64e

import resilient_class
import resilient_protocol

// The resilient witnesses of a conformance refer to the requirement
// descriptors of the protocol, and the method overrides of a class refer to
// the method descriptors of its superclass. References to descriptors in
// another module go through GOT equivalents, which are signed.

public struct Conformer: ProtocolWithRequirements {
  public typealias T = Int
  public func first() {}
  public func second() {}
}

open class Subclass: ResilientOutsideParent {
  open override func method() {}
}

// CHECK: @"got.$s1T18resilient_protocol24ProtocolWithRequirementsPTl" = private unnamed_addr constant ptr @"$s1T18resilient_protocol24ProtocolWithRequirementsPTl.ptrauth"
// CHECK: @"$s1T18resilient_protocol24ProtocolWithRequirementsPTl.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$s1T18resilient_protocol24ProtocolWithRequirementsPTl", i32 2, i64 ptrtoint (ptr @"got.$s1T18resilient_protocol24ProtocolWithRequirementsPTl" to i64), i64 32076 }, section "llvm.ptrauth"
// CHECK: @"got.$s18resilient_protocol24ProtocolWithRequirementsP5firstyyFTq" = private unnamed_addr constant ptr @"$s18resilient_protocol24ProtocolWithRequirementsP5firstyyFTq.ptrauth"
// CHECK: @"$s18resilient_protocol24ProtocolWithRequirementsP5firstyyFTq.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$s18resilient_protocol24ProtocolWithRequirementsP5firstyyFTq", i32 2, i64 ptrtoint (ptr @"got.$s18resilient_protocol24ProtocolWithRequirementsP5firstyyFTq" to i64), i64 32076 }, section "llvm.ptrauth"

// CHECK: @"got.$s15resilient_class22ResilientOutsideParentC6methodyyFTq" = private unnamed_addr constant ptr @"$s15resilient_class22ResilientOutsideParentC6methodyyFTq.ptrauth"
// CHECK: @"$s15resilient_class22ResilientOutsideParentC6methodyyFTq.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$s15resilient_class22ResilientOutsideParentC6methodyyFTq", i32 2, i64 ptrtoint (ptr @"got.$s15resilient_class22ResilientOutsideParentC6methodyyFTq" to i64), i64 26458 }, section "llvm.ptrauth"
// CHECK: @"got.$s15resilient_class22ResilientOutsideParentCACycfCTq" = private unnamed_addr constant ptr @"$s15resilient_class22ResilientOutsideParentCACycfCTq.ptrauth"
// CHECK: @"$s15resilient_class22ResilientOutsideParentCACycfCTq.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr @"$s15resilient_class22ResilientOutsideParentCACycfCTq", i32 2, i64 ptrtoint (ptr @"got.$s15resilient_class22ResilientOutsideParentCACycfCTq" to i64), i64 26458 }, section "llvm.ptrauth"
