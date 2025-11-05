// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name ResilientTypes -emit-module-path %t/ResilientTypes.swiftmodule -enable-library-evolution %S/Inputs/builtin_borrow_ResilientTypes.swift
// RUN: %target-swift-emit-silgen -enable-experimental-feature BuiltinModule -I %t %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule

import Builtin
import ResilientTypes

struct NonGeneric {
	var x: Int
}

struct NonGenericAO {
	var x: Any
}

struct Fixed<T> {
	var x: Int
}

struct Dependent<T> {
	var x: T
}

struct ContainsResilient {
	var x: Resilient
}

// Borrow is loadable when its referent has a fixed layout. Otherwise, it's
// address-only.

// CHECK-LABEL: sil {{.*}}@$s{{.*}}10nongeneric{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<NonGeneric>)
func nongeneric(_: Builtin.Borrow<NonGeneric>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}13nongeneric_ao{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<NonGenericAO>)
func nongeneric_ao(_: Builtin.Borrow<NonGenericAO>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}9resilient{{.*}} :
// CHECK-SAME: $@convention(thin) (@in_guaranteed Builtin.Borrow<Resilient>)
func resilient(_: Builtin.Borrow<Resilient>) {} // address

// CHECK-LABEL: sil {{.*}}@$s{{.*}}18contains_resilient{{.*}} :
// CHECK-SAME: $@convention(thin) (@in_guaranteed Builtin.Borrow<ContainsResilient>)
func contains_resilient(_: Builtin.Borrow<ContainsResilient>) {} // address

// CHECK-LABEL: sil {{.*}}@$s{{.*}}10some_fixed{{.*}} :
// CHECK-SAME: $@convention(thin) <T> (Builtin.Borrow<Fixed<T>>)
func some_fixed<T>(_: Builtin.Borrow<Fixed<T>>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}11known_fixed{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<Fixed<NonGeneric>>)
func known_fixed(_: Builtin.Borrow<Fixed<NonGeneric>>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}14known_fixed_ao{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<Fixed<NonGenericAO>>)
func known_fixed_ao(_: Builtin.Borrow<Fixed<NonGenericAO>>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}15resilient_fixed{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<Fixed<Resilient>>)
func resilient_fixed(_: Builtin.Borrow<Fixed<Resilient>>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}14some_dependent{{.*}} :
// CHECK-SAME: $@convention(thin) <T> (@in_guaranteed Builtin.Borrow<Dependent<T>>)
func some_dependent<T>(_: Builtin.Borrow<Dependent<T>>) {} // address

// CHECK-LABEL: sil {{.*}}@$s{{.*}}15known_dependent{{.*}} :
// CHECK-SAME: $@convention(thin) (Builtin.Borrow<Dependent<NonGeneric>>)
func known_dependent(_: Builtin.Borrow<Dependent<NonGeneric>>) {} // loadable

// CHECK-LABEL: sil {{.*}}@$s{{.*}}19resilient_dependent{{.*}} :
// CHECK-SAME: $@convention(thin) (@in_guaranteed Builtin.Borrow<Dependent<Resilient>>)
func resilient_dependent(_: Builtin.Borrow<Dependent<Resilient>>) {} // address

