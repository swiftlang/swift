// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.15 -target-variant %target-cpu-apple-ios13.1-macabi | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-ios13.1-macabi -target-variant %target-cpu-apple-macosx10.15 | %FileCheck %s


// REQUIRES: OS=macosx || OS=maccatalyst

// CHECK-LABEL: sil{{.+}}@main{{.*}} {

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

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 11
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 11.0, iOS 51.1.2, *) {
}

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 16
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 19
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(macOS 16.0, iOS 19.0, *) {
}

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 26
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 26
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(macOS 26.0, iOS 26.0, *) {
}
