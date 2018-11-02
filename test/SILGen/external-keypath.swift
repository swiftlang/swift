// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/ExternalKeyPaths.swiftmodule -module-name ExternalKeyPaths %S/Inputs/ExternalKeyPaths.swift
// RUN: %target-swift-frontend -enable-key-path-resilience -emit-silgen -I %t %s | %FileCheck %s

import ExternalKeyPaths

struct Local {
  var x: Int
  var y: String
}

// CHECK-LABEL: sil hidden @{{.*}}16externalKeyPaths
func externalKeyPaths<T: Hashable, U>(_ x: T, _ y: U, _ z: Int) {
  // CHECK: keypath $WritableKeyPath<External<Int>, Int>, (root $External<Int>; external #External.property<Int> : $Int)
  _ = \External<Int>.property

  // CHECK: keypath $WritableKeyPath<External<Int>, Int>, (root $External<Int>; external #External.intProperty<Int> : $Int)
  _ = \External<Int>.intProperty

  // CHECK: keypath $WritableKeyPath<External<T>, T>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; external #External.property<T> : $τ_0_0) <T, U> 
  _ = \External<T>.property

  // CHECK: keypath $WritableKeyPath<External<T>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; external #External.intProperty<T> : $Int) <T, U>
  _ = \External<T>.intProperty

  // CHECK: keypath $WritableKeyPath<External<U>, U>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; external #External.property<U> : $τ_0_1) <T, U>
  _ = \External<U>.property

  // CHECK: keypath $WritableKeyPath<External<U>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; external #External.intProperty<U> : $Int) <T, U>
  _ = \External<U>.intProperty

  // CHECK: keypath $KeyPath<External<Int>, Int>, (root $External<Int>; external #External.subscript<Int, Int>[%$0 : $Int : $Int] : $Int, indices_equals @{{.*}}) (%2)
  _ = \External<Int>.[z]

  // CHECK: keypath $KeyPath<External<T>, T>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_0>; external #External.subscript<T, T>[%$0 : $τ_0_0 : $*τ_0_0] : $τ_0_0, indices_equals @{{.*}}) <T, U> ({{.*}})
  _ = \External<T>.[x]

  // CHECK: keypath $KeyPath<External<U>, U>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (root $External<τ_0_1>; external #External.subscript<U, T>[%$0 : $τ_0_0 : $*τ_0_0] : $τ_0_1, indices_equals @{{.*}}) <T, U> ({{.*}})
  _ = \External<U>.[x]

  // CHECK: keypath $KeyPath<External<Local>, Int>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (
  // CHECK-SAME: root $External<Local>;
  // CHECK-SAME: external #External.subscript<Local, T>[%$0 : $τ_0_0 : $*τ_0_0] : $Local, indices_equals @{{.*}};
  // CHECK-SAME: stored_property #Local.x : $Int) <T, U> ({{.*}})
  _ = \External<Local>.[x].x

  // CHECK: keypath $KeyPath<External<Local>, String>, <τ_0_0, τ_0_1 where τ_0_0 : Hashable> (
  // CHECK-SAME: root $External<Local>;
  // CHECK-SAME: external #External.subscript<Local, T>[%$0 : $τ_0_0 : $*τ_0_0] : $Local, indices_equals @{{.*}};
  // CHECK-SAME: stored_property #Local.y : $String) <T, U> ({{.*}})
  _ = \External<Local>.[x].y

  // CHECK: keypath $KeyPath<ExternalEmptySubscript, Int>, (
  // CHECK-SAME: root $ExternalEmptySubscript;
  // CHECK-SAME: external #ExternalEmptySubscript.subscript : $Int)
  _ = \ExternalEmptySubscript.[]
}
