// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-resilience -emit-module -o %t/ExternalKeyPaths.swiftmodule -module-name ExternalKeyPaths %S/Inputs/ExternalKeyPaths.swift
// RUN: %target-swift-emit-silgen -swift-version 5 -I %t %s | %FileCheck %s

import ExternalKeyPaths

struct Local {
  var x: Int
  var y: String
}

// CHECK-LABEL: sil hidden @{{.*}}16externalKeyPaths
func externalKeyPaths<T: Hashable, U>(_ x: T, _ y: U, _ z: Int) {
  // CHECK: keypath $WritableKeyPath<External<Int>, Int>, (root $External<Int>; {{.*}} external #External.property<Int>)
  _ = \External<Int>.property

  // CHECK: keypath $WritableKeyPath<External<Int>, Int>, (root $External<Int>; {{.*}} external #External.intProperty<Int>)
  _ = \External<Int>.intProperty

  // CHECK: keypath $WritableKeyPath<External<T>, T>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; {{.*}} external #External.property<T>) <T, U> 
  _ = \External<T>.property

  // CHECK: keypath $WritableKeyPath<External<T>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; {{.*}} external #External.intProperty<T>) <T, U>
  _ = \External<T>.intProperty

  // CHECK: keypath $WritableKeyPath<External<U>, U>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; {{.*}} external #External.property<U>) <T, U>
  _ = \External<U>.property

  // CHECK: keypath $WritableKeyPath<External<U>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; {{.*}} external #External.intProperty<U>) <T, U>
  _ = \External<U>.intProperty

  // CHECK: keypath $KeyPath<External<Int>, Int>, (root $External<Int>; {{.*}} external #External.subscript<Int, Int>) (%2)
  _ = \External<Int>.[z]

  // CHECK: keypath $KeyPath<External<T>, T>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; {{.*}} external #External.subscript<T, T>) <T, U> ({{.*}})
  _ = \External<T>.[x]

  // CHECK: keypath $KeyPath<External<U>, U>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; {{.*}} external #External.subscript<U, T>) <T, U> ({{.*}})
  _ = \External<U>.[x]

  // CHECK: keypath $KeyPath<External<Local>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (
  // CHECK-SAME: root $External<Local>;
  // CHECK-SAME: external #External.subscript<Local, T>
  // CHECK-SAME: stored_property #Local.x : $Int) <T, U> ({{.*}})
  _ = \External<Local>.[x].x

  // CHECK: keypath $KeyPath<External<Local>, String>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (
  // CHECK-SAME: root $External<Local>;
  // CHECK-SAME: external #External.subscript<Local, T>
  // CHECK-SAME: stored_property #Local.y : $String) <T, U> ({{.*}})
  _ = \External<Local>.[x].y

  // CHECK: keypath $KeyPath<ExternalEmptySubscript, Int>, (
  // CHECK-SAME: root $ExternalEmptySubscript;
  // CHECK-SAME: external #ExternalEmptySubscript.subscript
  _ = \ExternalEmptySubscript.[]

  // CHECK: keypath $KeyPath<External<Int>, Int>, (
  // CHECK-SAME: root $External<Int>;
  // CHECK-SAME: gettable_property
  // CHECK-SAME: external #External.privateSetProperty
  _ = \External<Int>.privateSetProperty

  // CHECK: keypath $KeyPath<External<Int>, Int>, (
  // CHECK-SAME: root $External<Int>;
  // CHECK-SAME: gettable_property
  // CHECK-SAME: external #External.subscript
  _ = \External<Int>.[privateSet: 0]
}

// CHECK-LABEL: sil hidden @{{.*}}testProtocolRequirement
func testProtocolRequirement<T: ExternalProto>(_: T.Type) {
  // CHECK: keypath $WritableKeyPath<T, Int>,
  // CHECK-NOT: external #ExternalProto.protoReqt
  _ = \T.protoReqt
}
