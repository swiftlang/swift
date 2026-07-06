// RUN: %target-swift-emit-silgen \
// RUN: -enable-experimental-feature BuiltinModule \
// RUN: -enable-experimental-feature AddressableTypes \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: -verify %s

// TODO: Support Builtin.makeBorrow(Builtin.borrowAt(referent)). See
// SILGenFunction::tryEmitAddressableParameterAsAddress.

// UN: %target-swift-emit-silgen -module-name main \
// UN: -enable-experimental-feature BuiltinModule \
// UN: -enable-experimental-feature AddressableType \
// UN: -enable-experimental-feature Lifetimes \
// UN: -I %t %s | %FileCheck %s

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Lifetimes

import Builtin

struct NonGeneric {
  var x: AnyObject
}

struct NonGenericAO {
  var x: Any
}

@_addressableForDependencies
struct AFD { var x: AnyObject }

@_addressableForDependencies
struct AFDTrivial { var x: Int }

// HECK-LABEL: sil{{.*}} @$s{{.*}}06value_B12_make_borrow
// HECK:       bb0([[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         [[LOAD:%.*]] = load [trivial] [[ADDR]]
// HECK:         [[BORROW:%.*]] = make_borrow [[LOAD]]
// HECK:         return [[BORROW]]
@_lifetime(borrow referent)
func trivial_make_borrow(_ referent: Builtin.RawPointer) -> Builtin.Borrow<Int> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent))
}

// HECK-LABEL: sil{{.*}} @$s{{.*}}06value_B12_make_borrow
// HECK:       bb0([[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         [[LOAD:%.*]] = load_borrow [[ADDR]]
// HECK:         [[BORROW:%.*]] = make_borrow [[LOAD]]
// HECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_value_make_borrow(_ referent: Builtin.RawPointer) -> Builtin.Borrow<NonGeneric> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent))
}

// HECK-LABEL: sil{{.*}} @$s{{.*}}22value_address_borrowat
// HECK:       bb0([[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         [[BORROW:%.*]] = make_addr_borrow [[ADDR]]
// HECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_borrowat(_ referent: Builtin.RawPointer) -> Builtin.Borrow<NonGenericAO> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent)) // expected-error{{INTERNAL ERROR: feature not implemented: Builtin.borrowAt must be direclty returned from a borrow accessor}}
}

// HECK-LABEL: sil{{.*}} @$s{{.*}}26value_address_borrowat_afd
// HECK:       bb0([[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         [[BORROW:%.*]] = make_addr_borrow [[ADDR]]
// HECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_borrowat_afd(_ referent: Builtin.RawPointer) -> Builtin.Borrow<AFD> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent)) // expected-error{{INTERNAL ERROR: feature not implemented: Builtin.borrowAt must be direclty returned from a borrow accessor}}
}

// HECK-LABEL: sil{{.*}} @$s{{.*}}34value_address_borrowat_afd_trivial
// HECK:       bb0([[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         [[BORROW:%.*]] = make_addr_borrow [[ADDR]]
// HECK:         return [[BORROW]]
@_lifetime(borrow referent)
func value_address_borrowat_afd_trivial(_ referent: Builtin.RawPointer) -> Builtin.Borrow<AFDTrivial> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent)) // expected-error{{INTERNAL ERROR: feature not implemented: Builtin.borrowAt must be direclty returned from a borrow accessor}}
}

// HECK-LABEL: sil{{.*}} @$s{{.*}}08address_B9_borrowat
// HECK:       bb0([[RESULT:%.*]] : $*Builtin.Borrow{{.*}}, [[REFERENT:%.*]] :
// HECK:         [[ADDR:%.*]] = pointer_to_address [[REFERENT]]
// HECK:         make_addr_borrow [[RESULT]] with [[ADDR]]
@_lifetime(borrow referent)
func address_address_borrowat<T>(_ referent: Builtin.RawPointer) -> Builtin.Borrow<T> {
  return Builtin.makeBorrow(Builtin.borrowAt(referent)) // expected-error{{INTERNAL ERROR: feature not implemented: Builtin.borrowAt must be direclty returned from a borrow accessor}}
}
