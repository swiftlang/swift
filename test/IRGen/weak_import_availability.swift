// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_availability_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_availability_helper.swift -enable-resilience
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx

import weak_import_availability_helper

public func useConditionallyAvailableCase(e: AlwaysAvailableEnum) {
  switch e {
    case .alwaysAvailableCase: break
    case .conditionallyAvailableCase: break
  }
}

// CHECK-LABEL: @"$s31weak_import_availability_helper19AlwaysAvailableEnumO013conditionallyF4CaseyA2CmFWC" = extern_weak constant i32

func useConformance<T : AlwaysAvailableProtocol>(_: T.Type) {}

@available(macOS 10.50, *)
public func useConditionallyAvailableConformance() {
  useConformance(AlwaysAvailableStruct.self)
}

// FIXME: We reference the witness table directly -- that's a bug since the module is resilient. Should reference the
// conformance descriptor instead.

// CHECK-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA0eF8ProtocolAAWP" = extern_weak global i8*

@available(macOS 10.50, *)
public func callConditionallyAvailableFunction() {
  conditionallyAvailableFunction()
}

// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper30conditionallyAvailableFunctionyyF"()

@available(macOS 10.50, *)
public func useConditionallyAvailableGlobal() {
  _ = conditionallyAvailableGlobal
  conditionallyAvailableGlobal = 0
  conditionallyAvailableGlobal += 1
}

// CHECK-LABEL: declare extern_weak swiftcc i64 @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivg"()

// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivs"(i64)

func blackHole<T>(_: T) {}

@available(macOS 10.50, *)
public func useConditionallyAvailableStruct() {
  blackHole(ConditionallyAvailableStruct.self)
}

// CHECK-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s31weak_import_availability_helper28ConditionallyAvailableStructVMa"(i64)

@available(macOS 10.50, *)
public func useConditionallyAvailableMethod(s: ConditionallyAvailableStruct) {
  s.conditionallyAvailableMethod()
}

// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28ConditionallyAvailableStructV013conditionallyF6MethodyyF"(%swift.opaque* noalias nocapture swiftself)
