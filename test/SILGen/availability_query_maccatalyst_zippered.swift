// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.52 -target-variant %target-cpu-apple-ios50.0-macabi | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-ios50.0-macabi -target-variant %target-cpu-apple-macosx10.52 | %FileCheck %s

// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.14.4 -target-variant %target-cpu-apple-ios50.0-macabi | %FileCheck %s --check-prefix=CHECK-BACKDEPLOY-MAC
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-ios50.0-macabi -target-variant %target-cpu-apple-macosx10.14.4 | %FileCheck %s --check-prefix=CHECK-BACKDEPLOY-MAC

// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.15 -target-variant %target-cpu-apple-ios50.0-macabi | %FileCheck %s --check-prefix=CHECK-DEPLOY10_15-MAC
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-ios50.0-macabi -target-variant %target-cpu-apple-macosx10.15 | %FileCheck %s --check-prefix=CHECK-DEPLOY10_15-MAC

// REQUIRES: OS=macosx || OS=maccatalyst

// CHECK-LABEL: sil{{.+}}@main{{.*}} {

// When building zippered, we emit a call to a special version
// checking entrypoint that takes both an iOS version and a
// Mac version. The entrypoint will compare against the iOS
// version in an macCatalyst process and against the macOS version
// in a macOS process.

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 8
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.53.8, iOS 51.1.2, *) {
}

// Make sure macCatalyst wins over iOS.

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 8
// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 3
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 5
// CHECK: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
if #available(OSX 10.53.8, iOS 51.1.2, macCatalyst 52.3.5, *) {
}

// CHECK: [[TRUE:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK: cond_br [[TRUE]]
// The '*' matches for both macOS and macCatalyst, so the condition
// should always be true.
if #available(tvOS 9.0, *) {
}

// CHECK: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 3
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// The '*' matches for iOS, so we only need to check to check the
// macOS version and thus use the primary target version check
// entrypoint.
if #available(macOS 10.54.3, *) {
}

// CHECK: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 7
// CHECK: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK: [[QUERY_FUNC:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// The '*' matches for macOS, so we only need to check to check the
// iOS version and thus use the variant target version check
// entrypoint.
if #available(iOS 54.7.2, *) {
}

// The ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF entry point only exists
// on macOS 10.15+, so apply the optimization to omit the macOS version if the macOS
// deployment target is earlier than 10.15. In this case, use the
// ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF
// entry point, which is present in 10.14.4 and is part of the Swift 5.0 ABI.

// CHECK-BACKDEPLOY-MAC-LABEL: // backdeployMacStar()
// CHECK-BACKDEPLOY-MAC: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK-BACKDEPLOY-MAC: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 14
// CHECK-BACKDEPLOY-MAC: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 4
// CHECK-BACKDEPLOY-MAC: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK-BACKDEPLOY-MAC: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 7
// CHECK-BACKDEPLOY-MAC: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-BACKDEPLOY-MAC: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-BACKDEPLOY-MAC: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-DEPLOY10_15-MAC-LABEL: // backdeployMacStar()
// CHECK-DEPLOY10_15-MAC: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK-DEPLOY10_15-MAC: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 7
// CHECK-DEPLOY10_15-MAC: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-DEPLOY10_15-MAC: [[QUERY_FUNC:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-DEPLOY10_15-MAC: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
func backdeployMacStar() {
  if #available(iOS 54.7.2, *) {
  }
}

// CHECK-BACKDEPLOY-MAC-LABEL: // backdeployMacExplicit()
// CHECK-BACKDEPLOY-MAC: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK-BACKDEPLOY-MAC: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 14
// CHECK-BACKDEPLOY-MAC: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 5
// CHECK-BACKDEPLOY-MAC: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK-BACKDEPLOY-MAC: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 7
// CHECK-BACKDEPLOY-MAC: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-BACKDEPLOY-MAC: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-BACKDEPLOY-MAC: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-DEPLOY10_15-MAC-LABEL: // backdeployMacExplicit()
// CHECK-DEPLOY10_15-MAC: [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK-DEPLOY10_15-MAC: [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 14
// CHECK-DEPLOY10_15-MAC: [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 5
// CHECK-DEPLOY10_15-MAC: [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 54
// CHECK-DEPLOY10_15-MAC: [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 7
// CHECK-DEPLOY10_15-MAC: [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-DEPLOY10_15-MAC: [[FUNC:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK-DEPLOY10_15-MAC: [[QUERY_RESULT:%.*]] = apply [[FUNC]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
func backdeployMacExplicit() {
  if #available(iOS 54.7.2, macOS 10.14.5, *) {
  }
}
