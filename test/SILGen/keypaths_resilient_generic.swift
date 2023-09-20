// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift

// RUN: %target-swift-emit-silgen %s -I %t -enable-library-evolution | %FileCheck %s

import resilient_class

open class MySubclass<T> : ResilientOutsideParent {
  public final var storedProperty: T? = nil
}

open class ConcreteSubclass : MySubclass<Int> {
  public final var anotherStoredProperty: Int? = nil
}

// CHECK-LABEL: sil shared [thunk] [ossa] @$s26keypaths_resilient_generic10MySubclassC14storedPropertyxSgvplACyxGTK : $@convention(keypath_accessor_getter) <T> (@in_guaranteed MySubclass<T>) -> @out Optional<T> {

// CHECK-LABEL: sil shared [thunk] [ossa] @$s26keypaths_resilient_generic10MySubclassC14storedPropertyxSgvplACyxGTk : $@convention(keypath_accessor_setter) <T> (@in_guaranteed Optional<T>, @in_guaranteed MySubclass<T>) -> () {

// CHECK:      sil_property #MySubclass.storedProperty<τ_0_0> (
// CHECK-SAME:   settable_property $Optional<τ_0_0>,
// CHECK-SAME:   id ##MySubclass.storedProperty,
// CHECK-SAME:   getter @$s26keypaths_resilient_generic10MySubclassC14storedPropertyxSgvplACyxGTK : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed MySubclass<τ_0_0>) -> @out Optional<τ_0_0>,
// CHECK-SAME:   setter @$s26keypaths_resilient_generic10MySubclassC14storedPropertyxSgvplACyxGTk : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed Optional<τ_0_0>, @in_guaranteed MySubclass<τ_0_0>) -> ()
// CHECK-SAME: )

// CHECK:      sil_property #ConcreteSubclass.anotherStoredProperty (
// CHECK-SAME:   stored_property #ConcreteSubclass.anotherStoredProperty : $Optional<Int>
// CHECK-SAME: )
