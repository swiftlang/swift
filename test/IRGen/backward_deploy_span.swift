// The symbols of the types that are back deployed by the Span compatibility
// library are strongly linked by default (CHECK-STRONG). They are weakly linked
// (CHECK-WEAK) when -weak-link-span-compatibility-lib is specified and the
// deployment target predates the OS releases that introduced those types.
//
// Deployment targets with a runtime that cannot demangle these types must
// access their metadata with accessor calls (CHECK-ACCESSOR), while newer
// runtimes instantiate metadata from mangled names (CHECK-DEMANGLE).

// RUN: %target-swift-frontend -target %target-swift-5.5-abi-triple -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-ACCESSOR,CHECK-STRONG,CHECK-ACCESSOR-STRONG
// RUN: %target-swift-frontend -target %target-swift-5.5-abi-triple -weak-link-span-compatibility-lib -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-ACCESSOR,CHECK-WEAK,CHECK-ACCESSOR-WEAK
// RUN: %target-swift-frontend -target %target-swift-6.1-abi-triple -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-DEMANGLE,CHECK-STRONG,CHECK-DEMANGLE-STRONG
// RUN: %target-swift-frontend -target %target-swift-6.1-abi-triple -weak-link-span-compatibility-lib -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-DEMANGLE,CHECK-WEAK,CHECK-DEMANGLE-WEAK
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-DEMANGLE,CHECK-STRONG,CHECK-DEMANGLE-STRONG
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple -weak-link-span-compatibility-lib -emit-ir -o - -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-DEMANGLE,CHECK-STRONG,CHECK-DEMANGLE-STRONG

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// CHECK-WEAK-DAG: @"$ss7RawSpanVN" = extern_weak global
// CHECK-WEAK-DAG: @"$ss14MutableRawSpanVN" = extern_weak global
// CHECK-WEAK-DAG: @"$ss13OutputRawSpanVN" = extern_weak global
// CHECK-STRONG-DAG: @"$ss7RawSpanVN" = external global
// CHECK-STRONG-DAG: @"$ss14MutableRawSpanVN" = external global
// CHECK-STRONG-DAG: @"$ss13OutputRawSpanVN" = external global

// CHECK-DEMANGLE-WEAK-DAG: @"$ss4SpanVMn" = extern_weak global
// CHECK-DEMANGLE-WEAK-DAG: @"$ss11MutableSpanVMn" = extern_weak global
// CHECK-DEMANGLE-WEAK-DAG: @"$ss10OutputSpanVMn" = extern_weak global
// CHECK-DEMANGLE-STRONG-DAG: @"$ss4SpanVMn" = external global
// CHECK-DEMANGLE-STRONG-DAG: @"$ss11MutableSpanVMn" = external global
// CHECK-DEMANGLE-STRONG-DAG: @"$ss10OutputSpanVMn" = external global

func useGenericMetatype(_: any (~Copyable & ~Escapable).Type) { }

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span11testSpanIntyyF"()
func testSpanInt() {
  // CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss4SpanVySiGMa"
  // CHECK-DEMANGLE: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$ss4SpanVySiGMD")
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(Span<Int>.self)
}

// CHECK-ACCESSOR-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$ss4SpanVySiGMa"
// CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss4SpanVMa"({{i32|i64}} %0, ptr @"$sSiN")
// CHECK-ACCESSOR-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss4SpanVMa"
// CHECK-ACCESSOR-STRONG: declare swiftcc %swift.metadata_response @"$ss4SpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span18testMutableSpanIntyyF"()
func testMutableSpanInt() {
  // CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss11MutableSpanVySiGMa"
  // CHECK-DEMANGLE: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$ss11MutableSpanVySiGMD")
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(MutableSpan<Int>.self)
}

// CHECK-ACCESSOR-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$ss11MutableSpanVySiGMa"
// CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"({{i32|i64}} %0, ptr @"$sSiN")
// CHECK-ACCESSOR-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"
// CHECK-ACCESSOR-STRONG: declare swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span17testOutputSpanIntyyF"()
func testOutputSpanInt() {
  // CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss10OutputSpanVySiGMa"
  // CHECK-DEMANGLE: call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$ss10OutputSpanVySiGMD")
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(OutputSpan<Int>.self)
}

// CHECK-ACCESSOR-LABEL: define linkonce_odr hidden swiftcc %swift.metadata_response @"$ss10OutputSpanVySiGMa"
// CHECK-ACCESSOR: call swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"({{i32|i64}} %0, ptr @"$sSiN")
// CHECK-ACCESSOR-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"
// CHECK-ACCESSOR-STRONG: declare swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span15testSpanGenericyyxmlF"(
func testSpanGeneric<T>(_: T.Type) {
  // CHECK: call swiftcc %swift.metadata_response @"$ss4SpanVMa"({{i32|i64}} 0, ptr %T)
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(Span<T>.self)
}

// CHECK-DEMANGLE-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss4SpanVMa"
// CHECK-DEMANGLE-STRONG: declare swiftcc %swift.metadata_response @"$ss4SpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span22testMutableSpanGenericyyxmlF"(
func testMutableSpanGeneric<T>(_: T.Type) {
  // CHECK: call swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"({{i32|i64}} 0, ptr %T)
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(MutableSpan<T>.self)
}

// CHECK-DEMANGLE-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"
// CHECK-DEMANGLE-STRONG: declare swiftcc %swift.metadata_response @"$ss11MutableSpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span21testOutputSpanGenericyyxmlF"(
func testOutputSpanGeneric<T>(_: T.Type) {
  // CHECK: call swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"({{i32|i64}} 0, ptr %T)
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"
  useGenericMetatype(OutputSpan<T>.self)
}

// CHECK-DEMANGLE-WEAK: declare extern_weak swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"
// CHECK-DEMANGLE-STRONG: declare swiftcc %swift.metadata_response @"$ss10OutputSpanVMa"

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span11testRawSpanyyF"()
func testRawSpan() {
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"(ptr @"$ss7RawSpanVN")
  useGenericMetatype(RawSpan.self)
}

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span18testMutableRawSpanyyF"()
func testMutableRawSpan() {
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"(ptr @"$ss14MutableRawSpanVN")
  useGenericMetatype(MutableRawSpan.self)
}

// CHECK-LABEL: define hidden swiftcc void @"$s20backward_deploy_span17testOutputRawSpanyyF"()
func testOutputRawSpan() {
  // CHECK: call swiftcc void @"$s20backward_deploy_span18useGenericMetatypeyyypRi_s_Ri0_sXPXpF"(ptr @"$ss13OutputRawSpanVN")
  useGenericMetatype(OutputRawSpan.self)
}
