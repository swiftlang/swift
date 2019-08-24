// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weak_import_availability_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_availability_helper.swift -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s --check-prefix=CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.50 | %FileCheck %s --check-prefix=CHECK-NEW
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -target x86_64-apple-macosx10.60 | %FileCheck %s --check-prefix=CHECK-NEW

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64

import weak_import_availability_helper

public func useConditionallyAvailableCase(e: AlwaysAvailableEnum) {
  switch e {
    case .alwaysAvailableCase: break
    case .conditionallyAvailableCase: break
  }
}

// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper19AlwaysAvailableEnumO013conditionallyF4CaseyA2CmFWC" = extern_weak constant i32
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper19AlwaysAvailableEnumO013conditionallyF4CaseyA2CmFWC" = external constant i32

func useConformance<T : AlwaysAvailableProtocol>(_: T.Type) {}

@available(macOS 10.50, *)
public func useConditionallyAvailableConformance() {
  useConformance(AlwaysAvailableStruct.self)
}

// FIXME: We reference the witness table directly -- that's a bug since the module is resilient. Should reference the
// conformance descriptor instead.

// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA0eF8ProtocolAAWP" = extern_weak global i8*
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA0eF8ProtocolAAWP" = external global i8*

@available(macOS 10.50, *)
public func callConditionallyAvailableFunction() {
  conditionallyAvailableFunction()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper30conditionallyAvailableFunctionyyF"()
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper30conditionallyAvailableFunctionyyF"()

@available(macOS 10.50, *)
public func useConditionallyAvailableGlobal() {
  _ = conditionallyAvailableGlobal
  conditionallyAvailableGlobal = 0
  conditionallyAvailableGlobal += 1
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc i64 @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivg"()
// CHECK-NEW-LABEL: declare swiftcc i64 @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivg"()

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivs"(i64)
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivs"(i64)

func blackHole<T>(_: T) {}

@available(macOS 10.50, *)
public func useConditionallyAvailableStruct() {
  blackHole(ConditionallyAvailableStruct.self)
}

@available(macOS 10.50, *)
public func useConditionallyAvailableMethod(s: ConditionallyAvailableStruct) {
  s.conditionallyAvailableMethod()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28ConditionallyAvailableStructV013conditionallyF6MethodyyF"(%swift.opaque* noalias nocapture swiftself)
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper28ConditionallyAvailableStructV013conditionallyF6MethodyyF"(%swift.opaque* noalias nocapture swiftself)
