// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name ResilientTypes -emit-module-path %t/ResilientTypes.swiftmodule -enable-library-evolution %S/Inputs/builtin_borrow_ResilientTypes.swift
// RUN: %target-swift-emit-silgen -module-name main -enable-experimental-feature BuiltinModule -enable-experimental-feature AddressableTypes -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowAndMutateAccessors -I %t %s | %FileCheck %s
// RUN: %target-swift-emit-sil -module-name main -enable-experimental-feature BuiltinModule -enable-experimental-feature AddressableTypes -enable-experimental-feature Lifetimes -enable-experimental-feature BorrowAndMutateAccessors -I %t %s | %FileCheck --check-prefix=CHECK-POST-CLEANUP %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BorrowAndMutateAccessors

import Builtin
import ResilientTypes

struct NonGeneric {
	var x: AnyObject
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

//
// Builtin.makeBorrow
//

@_addressableForDependencies
struct AFD { var x: AnyObject }

@_addressableForDependencies
struct AFDTrivial { var x: Int }

// CHECK-LABEL: sil{{.*}} @$s{{.*}}06value_B12_make_borrow
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[BORROW:%.*]] = make_borrow [[REFERENT]]
// CHECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_value_make_borrow(_ referent: NonGeneric) -> Builtin.Borrow<NonGeneric> {
	return Builtin.makeBorrow(referent)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}25value_address_make_borrow
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[BORROW:%.*]] = make_addr_borrow [[REFERENT]]
// CHECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_make_borrow(_ referent: NonGenericAO) -> Builtin.Borrow<NonGenericAO> {
	return Builtin.makeBorrow(referent)
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}29value_address_make_borrow_afd
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[BORROW:%.*]] = make_addr_borrow [[REFERENT]]
// CHECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_make_borrow_afd(_ referent: AFD) -> Builtin.Borrow<AFD> {
	return Builtin.makeBorrow(referent)
}

func take_afd_borrow(_ borrow: Builtin.Borrow<AFD>) {}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}38value_address_make_borrow_afd_downward
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[ADDR_REFERENT:%.*]] = alloc_stack
// CHECK:         [[ADDR_BORROW:%.*]] = store_borrow [[REFERENT]] to [[ADDR_REFERENT]]
// CHECK:         [[BORROW:%.*]] = make_addr_borrow [[ADDR_BORROW]]
// CHECK:         apply {{.*}}([[BORROW]])
func value_address_make_borrow_afd_downward(_ referent: AFD) {
	take_afd_borrow(Builtin.makeBorrow(referent))
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}37value_address_make_borrow_afd_trivial
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[BORROW:%.*]] = make_addr_borrow [[REFERENT]]
// CHECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_make_borrow_afd_trivial(_ referent: AFDTrivial) -> Builtin.Borrow<AFDTrivial> {
	return Builtin.makeBorrow(referent)
}

func take_afd_trivial_borrow(_ borrow: Builtin.Borrow<AFDTrivial>) {}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}46value_address_make_borrow_afd_trivial_downward
// CHECK:       bb0([[REFERENT:%.*]] :
// CHECK:         [[ADDR_REFERENT:%.*]] = alloc_stack
// CHECK:         [[ADDR_BORROW:%.*]] = store_borrow [[REFERENT]] to [[ADDR_REFERENT]]
// CHECK:         [[BORROW:%.*]] = make_addr_borrow [[ADDR_BORROW]]
// CHECK:         apply {{.*}}([[BORROW]])
func value_address_make_borrow_afd_trivial_downward(_ referent: AFDTrivial) {
	take_afd_trivial_borrow(Builtin.makeBorrow(referent))
}

// CHECK-LABEL: sil{{.*}} @$s{{.*}}08address_B12_make_borrow
// CHECK:       bb0([[RESULT:%.*]] : $*Builtin.Borrow{{.*}}, [[REFERENT:%.*]] :
// CHECK:         init_borrow_addr [[RESULT]] with [[REFERENT]]
@_lifetime(borrow referent)
func address_address_make_borrow<T>(_ referent: T) -> Builtin.Borrow<T> {
	return Builtin.makeBorrow(referent)
}

struct BorrowTrivial: ~Escapable {
	var borrowed: Builtin.Borrow<Int>

	var value: Int {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}13BorrowTrivialV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_borrow [[BORROW]]
		// CHECK:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}
}

struct BorrowLoadable: ~Escapable {
	var borrowed: Builtin.Borrow<NonGeneric>

	var value: NonGeneric {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}14BorrowLoadableV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_borrow [[BORROW]]
		// CHECK:         [[UNCHECKED:%.*]] = unchecked_ownership [[REFERENT]]
		// CHECK:         end_borrow [[REFERENT]]
		// CHECK:         return [[UNCHECKED]]
		//
		// SILGenCleanup fixes this to the more principled `return_borrow`,
		// which after ownership elimination gives us:
		// CHECK-POST-CLEANUP-LABEL: sil{{.*}} @$s{{.*}}14BorrowLoadableV5value{{.*}}vb :
		// CHECK-POST-CLEANUP:       bb0([[STRUCT:%.*]] :
		// CHECK-POST-CLEANUP:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK-POST-CLEANUP:         [[REFERENT:%.*]] = dereference_borrow [[BORROW]]
		// CHECK-POST-CLEANUP:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}
}

struct BorrowAO: ~Escapable {
	var borrowed: Builtin.Borrow<NonGenericAO>

	var value: NonGenericAO {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}8BorrowAOV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_addr_borrow [[BORROW]]
		// CHECK:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}
}

struct BorrowAFD: ~Escapable {
	var borrowed: Builtin.Borrow<AFD>

	var value: AFD {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}9BorrowAFDV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_addr_borrow [[BORROW]]
		// CHECK:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}
}

struct BorrowAFDTrivial: ~Escapable {
	var borrowed: Builtin.Borrow<AFDTrivial>

	var value: AFDTrivial {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}16BorrowAFDTrivialV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_extract [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_addr_borrow [[BORROW]]
		// CHECK:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}
}

struct BorrowDep<T>: ~Escapable {
	var value: T {
		// CHECK-LABEL: sil{{.*}} @$s{{.*}}9BorrowDepV5value{{.*}}vb :
		// CHECK:       bb0([[STRUCT:%.*]] :
		// CHECK:         [[BORROW:%.*]] = struct_element_addr [[STRUCT]]
		// CHECK:         [[REFERENT:%.*]] = dereference_borrow_addr [[BORROW]]
		// CHECK:         return [[REFERENT]]
		borrow {
			return Builtin.dereferenceBorrow(borrowed)
		}
	}

	private var borrowed: Builtin.Borrow<T>
}
