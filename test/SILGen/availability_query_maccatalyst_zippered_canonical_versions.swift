// RUN: %target-swift-emit-silgen %s -target x86_64-apple-macosx10.52 -target-variant x86_64-apple-ios50.0-macabi | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target x86_64-apple-ios50.0-macabi -target-variant x86_64-apple-macosx10.52 | %FileCheck %s


// REQUIRES: OS=macosx || OS=maccatalyst

// CHECK-LABEL: sil{{.+}}@main{{.*}} {


// Test for the runtime non-canonical version hack for canonical macOS versioning.
// This will eventually change to be the correctly canonicalized version.

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 16
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.16, iOS 51.1.2, *) {
}
