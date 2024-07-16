// RUN: %target-swift-emit-silgen %s -target x86_64-apple-macosx10.52 -target-variant x86_64-apple-ios50.0-macabi | %FileCheck %s --check-prefixes MAC,CHECK
// RUN: %target-swift-emit-silgen %s -target x86_64-apple-ios50.0-macabi -target-variant x86_64-apple-macosx10.52 | %FileCheck %s --check-prefixes IOS,CHECK


// REQUIRES: maccatalyst_support

// CHECK-LABEL: sil{{.+}}@main{{.*}} {


// Test for the runtime non-canonical version hack for canonical macOS versioning.
// This will eventually change to be the correctly canonicalized version.

// MAC: [[TARGET_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// MAC: [[TARGET_MINOR:%.*]] = integer_literal $Builtin.Word, 16
// MAC: [[TARGET_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// MAC: [[VARIANT_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// MAC: [[VARIANT_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// MAC: [[VARIANT_PATCH:%.*]] = integer_literal $Builtin.Word, 2

// IOS: [[TARGET_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// IOS: [[TARGET_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// IOS: [[TARGET_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// IOS: [[VARIANT_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// IOS: [[VARIANT_MINOR:%.*]] = integer_literal $Builtin.Word, 16
// IOS: [[VARIANT_PATCH:%.*]] = integer_literal $Builtin.Word, 0

// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[TARGET_MAJOR]], [[TARGET_MINOR]], [[TARGET_PATCH]], [[VARIANT_MAJOR]], [[VARIANT_MINOR]], [[VARIANT_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.16, iOS 51.1.2, *) {
}
