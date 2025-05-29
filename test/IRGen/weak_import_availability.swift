// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx50 -emit-module -emit-module-path %t/weak_import_availability_helper.swiftmodule -parse-as-library %S/Inputs/weak_import_availability_helper.swift -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %s -I %t -unavailable-decl-optimization=none -emit-ir | %FileCheck %s --check-prefixes=CHECK,CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -unavailable-decl-optimization=none -emit-ir -target %target-cpu-apple-macosx50 | %FileCheck %s --check-prefixes=CHECK,CHECK-NEW
// RUN: %target-swift-frontend -primary-file %s -I %t -unavailable-decl-optimization=none -emit-ir -target %target-cpu-apple-macosx60 | %FileCheck %s --check-prefixes=CHECK,CHECK-NEW

// RUN: %target-swift-frontend -primary-file %s -I %t -unavailable-decl-optimization=none -emit-ir -target %target-cpu-apple-macosx50 -weak-link-at-target | %FileCheck %s --check-prefixes=CHECK,CHECK-OLD
// RUN: %target-swift-frontend -primary-file %s -I %t -unavailable-decl-optimization=none -emit-ir -target %target-cpu-apple-macosx60 -weak-link-at-target | %FileCheck %s --check-prefixes=CHECK,CHECK-NEW

// REQUIRES: OS=macosx

import weak_import_availability_helper

// AlwaysAvailableEnum.conditionallyAvailableCase enum case
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper19AlwaysAvailableEnumO013conditionallyF4CaseyA2CmFWC" = extern_weak constant
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper19AlwaysAvailableEnumO013conditionallyF4CaseyA2CmFWC" = external constant


// Protocol witness table for AlwaysAvailableStruct: AlwaysAvailableProtocol
// FIXME: We reference the witness table directly -- that's a bug since the
// module is resilient. Should reference the conformance descriptor instead.
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA0eF8ProtocolAAWP" = extern_weak global ptr
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA0eF8ProtocolAAWP" = external global ptr


// Protocol witness table for AlwaysAvailableStruct: UnavailableProtocol
// CHECK-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructVAA19UnavailableProtocolAAWP" = extern_weak global ptr


// Opaque type descriptor for conditionallyAvailableOpaqueReturnFunction()
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper42conditionallyAvailableOpaqueReturnFunctionQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper42conditionallyAvailableOpaqueReturnFunctionQryFQOMQ" = external global %swift.type_descriptor


// Opaque type descriptor for AlwaysAvailableStruct.opaqueReturnMethodInConditionallyAvailableExtension()
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV033opaqueReturnMethodInConditionallyF9ExtensionQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV033opaqueReturnMethodInConditionallyF9ExtensionQryFQOMQ" = external global %swift.type_descriptor


// Opaque type descriptor for AlwaysAvailableStruct.conditionallyAvailableOpaqueReturnMethodInExtension()
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF29OpaqueReturnMethodInExtensionQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF29OpaqueReturnMethodInExtensionQryFQOMQ" = external global %swift.type_descriptor


// Opaque type descriptor for AlwaysAvailableStruct.conditionallyAvailableOpaqueReturnMethodInExplicitlyAvailableExtension()
// CHECK-OLD-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf30OpaqueReturnMethodInExplicitlyF9ExtensionQryFQOMQ" = extern_weak global %swift.type_descriptor
// CHECK-NEW-LABEL: @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf30OpaqueReturnMethodInExplicitlyF9ExtensionQryFQOMQ" = external global %swift.type_descriptor


public func useConditionallyAvailableCase(e: AlwaysAvailableEnum) {
  switch e {
    case .alwaysAvailableCase: break
    case .conditionallyAvailableCase: break
  }
}

func useConformance<T : AlwaysAvailableProtocol>(_: T.Type) {}

@available(macOS 50, *)
public func useConditionallyAvailableConformance() {
  useConformance(AlwaysAvailableStruct.self)
}

@available(macOS, unavailable)
func useUnavailableConformance<T : UnavailableProtocol>(_: T.Type) {}

@available(macOS, unavailable)
public func useUnavailableConformance() {
  useUnavailableConformance(AlwaysAvailableStruct.self)
}

@available(macOS 50, *)
public func callConditionallyAvailableFunction() {
  conditionallyAvailableFunction()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper30conditionallyAvailableFunctionyyF"()
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper30conditionallyAvailableFunctionyyF"()

@available(macOS, unavailable)
public func callUnavailableFunction() {
  unavailableFunction()
}

// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper19unavailableFunctionyyF"()

@available(macOS 50, *)
public func callConditionallyAvailableOpaqueReturnFunction() {
  blackHole(conditionallyAvailableOpaqueReturnFunction())
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper42conditionallyAvailableOpaqueReturnFunctionQryF"
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper42conditionallyAvailableOpaqueReturnFunctionQryF"

@available(macOS 50, *)
public func useConditionallyAvailableGlobal() {
  _ = conditionallyAvailableGlobal
  conditionallyAvailableGlobal = 0
  conditionallyAvailableGlobal += 1
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc i64 @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivg"()
// CHECK-NEW-LABEL: declare swiftcc i64 @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivg"()

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivs"(i64)
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivs"(i64)

// CHECK-OLD-LABEL: declare extern_weak swiftcc { ptr, ptr } @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivM"(ptr noalias dereferenceable(32))
// CHECK-NEW-LABEL: declare swiftcc { ptr, ptr } @"$s31weak_import_availability_helper28conditionallyAvailableGlobalSivM"(ptr noalias dereferenceable(32))

@available(macOS, unavailable)
public func useUnavailableGlobal() {
  _ = unavailableGlobal
  unavailableGlobal = 0
  unavailableGlobal += 1
}

// CHECK-LABEL: declare extern_weak swiftcc i64 @"$s31weak_import_availability_helper17unavailableGlobalSivg"()
// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper17unavailableGlobalSivs"(i64)
// CHECK-LABEL: declare extern_weak swiftcc { ptr, ptr } @"$s31weak_import_availability_helper17unavailableGlobalSivM"(ptr noalias dereferenceable(32))

func blackHole<T>(_: T) {}

@available(macOS 50, *)
public func useConditionallyAvailableStruct() {
  blackHole(ConditionallyAvailableStruct.self)
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s31weak_import_availability_helper28ConditionallyAvailableStructVMa"(i64)
// CHECK-NEW-LABEL: declare swiftcc %swift.metadata_response @"$s31weak_import_availability_helper28ConditionallyAvailableStructVMa"(i64)

@available(macOS 50, *)
public func useNestedConditionallyAvailableStruct() {
  blackHole(ConditionallyAvailableStruct.NestedStruct.self)
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s31weak_import_availability_helper28ConditionallyAvailableStructV06NestedG0VMa"(i64)
// CHECK-NEW-LABEL: declare swiftcc %swift.metadata_response @"$s31weak_import_availability_helper28ConditionallyAvailableStructV06NestedG0VMa"(i64)

@available(macOS 50, *)
public func useConditionallyAvailableMethod(s: ConditionallyAvailableStruct) {
  s.conditionallyAvailableMethod()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper28ConditionallyAvailableStructV013conditionallyF6MethodyyF"(ptr noalias swiftself)
// CHECK-NEW-LABEL: declare swiftcc void @"$s31weak_import_availability_helper28ConditionallyAvailableStructV013conditionallyF6MethodyyF"(ptr noalias swiftself)

@available(macOS 50, *)
public func useMethodInConditionallyAvailableExtension(s: AlwaysAvailableStruct) {
  s.methodInConditionallyAvailableExtension()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV021methodInConditionallyF9ExtensionyyF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV021methodInConditionallyF9ExtensionyyF"

@available(macOS 50, *)
public func useOpaqueReturnMethodInConditionallyAvailableExtension(s: AlwaysAvailableStruct) {
  blackHole(s.opaqueReturnMethodInConditionallyAvailableExtension())
}
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV033opaqueReturnMethodInConditionallyF9ExtensionQryF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV033opaqueReturnMethodInConditionallyF9ExtensionQryF"

@available(macOS 50, *)
public func useVarInConditionallyAvailableExtension(s: inout AlwaysAvailableStruct) {
  _ = s.varInConditionallyAvailableExtension
  s.varInConditionallyAvailableExtension = 0
  s.varInConditionallyAvailableExtension += 0
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivg"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivs"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivM"

// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivg"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivs"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV018varInConditionallyF9ExtensionSivM"

@available(macOS 50, *)
public func useConditionallyAvailableMethodInExtension(s: AlwaysAvailableStruct) {
  s.conditionallyAvailableMethodInExtension()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF17MethodInExtensionyyF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF17MethodInExtensionyyF"

@available(macOS 50, *)
public func useConditionallyAvailableOpaqueReturnMethodInExtension(s: AlwaysAvailableStruct) {
  blackHole(s.conditionallyAvailableOpaqueReturnMethodInExtension())
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF29OpaqueReturnMethodInExtensionQryF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF29OpaqueReturnMethodInExtensionQryF"

@available(macOS 50, *)
public func useConditionallyAvailableVarInExtension(s: inout AlwaysAvailableStruct) {
  _ = s.conditionallyAvailableVarInExtension
  s.conditionallyAvailableVarInExtension = 0
  s.conditionallyAvailableVarInExtension += 0
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivg"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivs"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivM"

// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivg"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivs"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF14VarInExtensionSivM"

@available(macOS 50, *)
public func useConditionallyAvailableMethodInExplicitlyAvailableExtension(s: AlwaysAvailableStruct) {
  s.conditionallyAvailableMethodInExplicitlyAvailableExtension()
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf18MethodInExplicitlyF9ExtensionyyF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf18MethodInExplicitlyF9ExtensionyyF"

@available(macOS 50, *)
public func useConditionallyAvailableOpaqueReturnMethodInExplicitlyAvailableExtension(s: AlwaysAvailableStruct) {
  blackHole(s.conditionallyAvailableOpaqueReturnMethodInExplicitlyAvailableExtension())
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf30OpaqueReturnMethodInExplicitlyF9ExtensionQryF"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyf30OpaqueReturnMethodInExplicitlyF9ExtensionQryF"


@available(macOS 50, *)
public func useConditionallyAvailableVarInExplicitlyAvailableExtension(s: inout AlwaysAvailableStruct) {
  _ = s.conditionallyAvailableVarInExplicitlyAvailablextension
  s.conditionallyAvailableVarInExplicitlyAvailablextension = 0
  s.conditionallyAvailableVarInExplicitlyAvailablextension += 0
}

// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivg"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivs"
// CHECK-OLD-LABEL: declare extern_weak swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivM"

// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivg"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivs"
// CHECK-NEW-LABEL: declare swiftcc {{.+}} @"$s31weak_import_availability_helper21AlwaysAvailableStructV013conditionallyF32VarInExplicitlyAvailablextensionSivM"

@available(macOS, unavailable)
public func useUnavailableStruct() {
  blackHole(UnavailableStruct.self)
}

// CHECK-LABEL: declare extern_weak swiftcc %swift.metadata_response @"$s31weak_import_availability_helper17UnavailableStructVMa"(i64)

@available(macOS, unavailable)
public func useUnavailableMethod(s: UnavailableStruct) {
  s.unavailableMethod()
}

// CHECK-LABEL: declare extern_weak swiftcc void @"$s31weak_import_availability_helper17UnavailableStructV17unavailableMethodyyF"(ptr noalias swiftself)
