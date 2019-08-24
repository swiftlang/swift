// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DOLD -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-OLD
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t.swiftdeps

// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/InterestingType.swift -DNEW -emit-reference-dependencies-path %t.swiftdeps -module-name main | %FileCheck %s -check-prefix=CHECK-NEW
// RUN: %FileCheck -check-prefix=CHECK-DEPS %s < %t.swiftdeps

struct Wrapper {
  fileprivate subscript() -> InterestingType { fatalError() }
}

// CHECK-OLD: sil_global @$s4main1x{{[^ ]+}} : $Int
// CHECK-NEW: sil_global @$s4main1x{{[^ ]+}} : $Double
public var x = Wrapper()[] + 0

// CHECK-DEPS-LABEL: depends-top-level:
// CHECK-DEPS: - "InterestingType"
