// REQUIRES: shell
// Also uses awk:
// XFAIL OS=windows

// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DOLD -disable-direct-intramodule-dependencies -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-OLD
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DNEW -disable-direct-intramodule-dependencies -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-NEW
// RUN: %S/../../Inputs/process_fine_grained_swiftdeps.sh %swift-dependency-tool %t.swiftdeps %t-processed.swiftdeps
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t-processed.swiftdeps

private struct Test : InterestingProto {}

// CHECK-OLD: sil_global @$s4main1x{{[^ ]+}} : $Int
// CHECK-NEW: sil_global @$s4main1x{{[^ ]+}} : $Double
public var x = Test().make() + 0

// CHECK-DEPS-DAG: topLevel interface  '' InterestingProto false

// CHECK-DEPS-DAG: member interface  4main{{8IntMaker|11DoubleMaker}}P make false

// CHECK-DEPS-DAG: nominal interface  4main{{8IntMaker|11DoubleMaker}}P '' false
