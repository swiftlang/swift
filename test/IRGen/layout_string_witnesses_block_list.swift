// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation -emit-ir -module-name Foo %s | %FileCheck %s
// RUN: %target-swift-frontend -target %target-future-triple -emit-ir -module-name Foo %s | %FileCheck %s --check-prefix=CHECK-DISABLED

// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "ShouldUseLayoutStringValueWitnesses:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - Foo" >> %t/blocklist.yml

// RUN: %target-swift-frontend -target %target-future-triple -enable-experimental-feature LayoutStringValueWitnesses -enable-layout-string-value-witnesses -emit-ir -blocklist-file %t/blocklist.yml -module-name Foo %s 2>&1 | %FileCheck %s --check-prefix=CHECK-BLOCKED

// REQUIRES: swift_feature_LayoutStringValueWitnesses
// REQUIRES: swift_feature_LayoutStringValueWitnessesInstantiation

// CHECK: type_layout_string

// CHECK-BLOCKED: note: Layout string value witnesses have been disabled for module 'Foo' through block list entry
// CHECK-BLOCKED-NOT: type_layout_string

// CHECK-DISABLED-NOT: note: Layout string value witnesses have been disabled for module 'Foo' through block list entry
// CHECK-DISABLED-NOT: type_layout_string
public struct Bar {
    let x: Int
    let y: AnyObject
}

// CHECK-BLOCKED-NOT: swift_enumFn_getEnumTag
public enum Foo {
    case a(AnyObject)
    case b(Int, AnyObject)
    case c
}
