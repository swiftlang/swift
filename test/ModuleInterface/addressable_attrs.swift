// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name attrs -enable-experimental-feature AddressableTypes
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs
// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: swift_feature_AddressableTypes

@_addressableForDependencies
public struct Foo {}
// CHECK: #if {{.*}} $AddressableTypes
// CHECK: @_addressableForDependencies public struct Foo
// CHECK: #else
// CHECK: {{^}}public struct Foo {
// CHECK: #endif
