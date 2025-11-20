// RUN: %target-swift-emit-silgen %s -target x86_64-apple-ios50.0-macabi | %FileCheck %s

// REQUIRES: OS=macosx || OS=maccatalyst

// CHECK-LABEL: sil{{.+}}@main{{.*}} {

// Under macCatalyst, iOS version numbers are used for availability checking we need
// to us the variant of the version checking function rather than the macOS checking
// function.

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[FUNC:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.53.8, iOS 51.1.2, *) {
}

// Make sure macCatalyst wins over iOS.

// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 3
// CHECK: [[FUNC:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.53.8, iOS 51.1.2, macCatalyst 53.2.3, *) {
}

// If there's no iOS or macCatalyst version, then the condition trivially
// evaluates to true

// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_br [[TRUE]]
if #available(OSX 10.53.8, *) {
}
