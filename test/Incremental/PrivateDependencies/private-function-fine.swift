// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DOLD -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-OLD
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DNEW -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-NEW
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

private func testParamType(_: InterestingType) {}

// CHECK-OLD: sil_global hidden @$s4main1x{{[^ ]+}} : ${{(@[a-zA-Z_]+ )?}}(Int) -> ()
// CHECK-NEW: sil_global hidden @$s4main1x{{[^ ]+}} : ${{(@[a-zA-Z_]+ )?}}(Double) -> ()
internal var x = testParamType

// CHECK-DEPS: topLevel interface  '' InterestingType false
