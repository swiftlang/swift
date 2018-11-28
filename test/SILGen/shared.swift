
// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle %s -disable-objc-attr-requires-foundation-module -enable-sil-ownership | %FileCheck %s

// REQUIRES: owned_parameters

class RefAggregate {}
struct ValueAggregate { let x = RefAggregate() }


// CHECK-LABEL: sil hidden @$s6shared0A10_arguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefG0ChtF : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
func shared_arguments(trivial : __shared Int, value : __shared ValueAggregate, ref : __shared RefAggregate) {
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate):
  // CHECK: [[COPY_VALUE_VAL:%[0-9]+]] = copy_value [[VALUE_VAL]] : $ValueAggregate
  // CHECK: [[COPY_REF_VAL:%[0-9]+]] = copy_value [[REF_VAL]] : $RefAggregate
  // CHECK: [[OWNED_FUNC:%[0-9]+]] = function_ref @$s6shared15owned_arguments7trivial5value3refySi_AA14ValueAggregateVAA03RefH0CtF
  // CHECK: {{%.*}} = apply [[OWNED_FUNC]]([[TRIVIAL_VAL]], [[COPY_VALUE_VAL]], [[COPY_REF_VAL]]) : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: } // end sil function '$s6shared0A10_arguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefG0ChtF'
  return owned_arguments(trivial: trivial, value: value, ref: ref)
}

// CHECK-LABEL: sil hidden @$s6shared15owned_arguments7trivial5value3refySi_AA14ValueAggregateVAA03RefH0CtF : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
func owned_arguments(trivial : Int, value : ValueAggregate, ref : RefAggregate) {
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @owned $ValueAggregate, [[REF_VAL:%[0-9]+]] : @owned $RefAggregate):
  // CHECK: [[BORROW_VALUE_VAL:%[0-9]+]] = begin_borrow [[VALUE_VAL]] : $ValueAggregate
  // CHECK: [[BORROW_REF_VAL:%[0-9]+]] = begin_borrow [[REF_VAL]] : $RefAggregate
  // CHECK: [[SHARED_FUNC:%[0-9]+]] = function_ref @$s6shared0A10_arguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefG0ChtF
  // CHECK: {{%.*}} = apply [[SHARED_FUNC]]([[TRIVIAL_VAL]], [[BORROW_VALUE_VAL]], [[BORROW_REF_VAL]])
  // CHECK: end_borrow [[BORROW_REF_VAL]] : $RefAggregate
  // CHECK: end_borrow [[BORROW_VALUE_VAL]] : $ValueAggregate
  // CHECK: destroy_value [[REF_VAL]] : $RefAggregate
  // CHECK: destroy_value [[VALUE_VAL]] : $ValueAggregate
  // CHECK: } // end sil function '$s6shared15owned_arguments7trivial5value3refySi_AA14ValueAggregateVAA03RefH0CtF'
  return shared_arguments(trivial: trivial, value: value, ref: ref)
}

// CHECK-LABEL: sil hidden @$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtF : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
func shared_argument_capture(trivial : __shared Int, value : __shared ValueAggregate, ref : __shared RefAggregate) {
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate):
  // CHECK: [[CLO_1:%[0-9]+]] = function_ref @$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU_
  // CHECK: {{%.*}} = apply [[CLO_1]]([[TRIVIAL_VAL]], [[VALUE_VAL]], [[REF_VAL]]) : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  _ = {
    return shared_arguments(trivial: trivial, value: value, ref: ref)
  }()
  
  // CHECK: [[CLO_2:%[0-9]+]] = function_ref @$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU0_ : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: {{%.*}} = apply [[CLO_2]]([[TRIVIAL_VAL]], [[VALUE_VAL]], [[REF_VAL]]) : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  _ = {
    return owned_arguments(trivial: trivial, value: value, ref: ref)
  }()
  
  // CHECK: } // end sil function '$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtF'
  
  // ======== FIRST CLOSURE ==========

  // CHECK-LABEL: sil private @$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU_ : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate):
  // CHECK: [[SHARED_CALL:%[0-9]+]] = function_ref @$s6shared0A10_arguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefG0ChtF : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: {{%.*}} = apply [[SHARED_CALL]]([[TRIVIAL_VAL]], [[VALUE_VAL]], [[REF_VAL]]) : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: } // end sil function '$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU_'
  
  // ======== SECOND CLOSURE ==========
  
  // CHECK-LABEL:  sil private @$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU0_ : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> () {
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate):
  // CHECK: [[COPY_BORROW_VALUE_VAL:%[0-9]+]] = copy_value [[VALUE_VAL]] : $ValueAggregate
  // CHECK: [[COPY_BORROW_REF_VAL:%[0-9]+]] = copy_value [[REF_VAL]] : $RefAggregate
  // CHECK: [[OWNED_CALL:%[0-9]+]] = function_ref @$s6shared15owned_arguments7trivial5value3refySi_AA14ValueAggregateVAA03RefH0CtF : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: {{%.*}} = apply [[OWNED_CALL]]([[TRIVIAL_VAL]], [[COPY_BORROW_VALUE_VAL]], [[COPY_BORROW_REF_VAL]]) : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: } // end sil function '$s6shared0A17_argument_capture7trivial5value3refySih_AA14ValueAggregateVhAA03RefH0ChtFyyXEfU0_'
}

// CHECK-LABEL: sil hidden @$s6shared0A20_to_owned_conversionyyySih_AA14ValueAggregateVhAA03RefF0ChtXEF : $@convention(thin) (@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
func shared_to_owned_conversion(_ f : (__shared Int, __shared ValueAggregate, __shared RefAggregate) -> Void) {
  // CHECK: bb0([[UNUSED_FUNC:%[0-9]+]] : $@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()):
  // CHECK: [[OWNED_THUNK:%[0-9]+]] = function_ref @$s6shared0A20_to_owned_conversionyyySih_AA14ValueAggregateVhAA03RefF0ChtXEFySi_AdFtXEfU_ : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_function [[OWNED_THUNK]]
  // CHECK: [[THICK_OWNED_THUNK:%[0-9]+]] = thin_to_thick_function [[CONVERT]] : $@convention(thin) @noescape (Int, @owned ValueAggregate, @owned RefAggregate) -> () to $@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: [[GUARANTEED_TO_OWNED_THUNK:%[0-9]+]] =  function_ref @$sSi6shared14ValueAggregateVAA03RefC0CIgyxx_SiAcEIegygg_TR : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate, @noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
  // CHECK: [[APPLIED_THUNK:%[0-9]+]] = partial_apply [callee_guaranteed] [[GUARANTEED_TO_OWNED_THUNK]]([[THICK_OWNED_THUNK]]) : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate, @noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [[APPLIED_THUNK]]
  // CHECK: [[RECUR_FN:%[0-9]+]] = function_ref @$s6shared0A20_to_owned_conversionyyySih_AA14ValueAggregateVhAA03RefF0ChtXEF : $@convention(thin) (@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
  // CHECK: {{%.*}} = apply [[RECUR_FN]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
  // CHECK-NOT: destroy_value [[UNUSED_FUNC]]
  // CHECK: } // end sil function '$s6shared0A20_to_owned_conversionyyySih_AA14ValueAggregateVhAA03RefF0ChtXEF'
  
  // ======== REABSTRACTION THUNK =========
  
  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sSi6shared14ValueAggregateVAA03RefC0CIgyxx_SiAcEIegygg_TR : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate, @noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate, [[FUNC:%[0-9]+]] : $@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()):
  // CHECK: [[COPY_VALUE_VAL:%[0-9]+]] = copy_value [[VALUE_VAL]] : $ValueAggregate
  // CHECK: [[COPY_REF_VAL:%[0-9]+]] = copy_value [[REF_VAL]] : $RefAggregate
  // CHECK: {{%.*}} = apply [[FUNC]]([[TRIVIAL_VAL]], [[COPY_VALUE_VAL]], [[COPY_REF_VAL]]) : $@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()
  // CHECK: } // end sil function '$sSi6shared14ValueAggregateVAA03RefC0CIgyxx_SiAcEIegygg_TR'
  
  return shared_to_owned_conversion { (trivial : Int, val : ValueAggregate, ref : RefAggregate) in }
}

// CHECK-LABEL: sil hidden @$s6shared09owned_to_A11_conversionyyySi_AA14ValueAggregateVAA03RefF0CtXEF : $@convention(thin) (@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
func owned_to_shared_conversion(_ f : (Int, ValueAggregate, RefAggregate) -> Void) {
  // CHECK: bb0([[UNUSED_FUNC:%[0-9]+]] : $@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()):
  // CHECK: [[SHARED_THUNK:%[0-9]+]] = function_ref @$s6shared09owned_to_A11_conversionyyySi_AA14ValueAggregateVAA03RefF0CtXEFySih_ADhAFhtXEfU_ : $@convention(thin) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_function [[SHARED_THUNK]]
  // CHECK: [[THICK_SHARED_THUNK:%[0-9]+]] = thin_to_thick_function [[CONVERT]] : $@convention(thin) @noescape (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> () to $@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: [[OWNED_TO_GUARANTEED_THUNK:%[0-9]+]] = function_ref @$sSi6shared14ValueAggregateVAA03RefC0CIgygg_SiAcEIegyxx_TR : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate, @noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
  // CHECK: [[APPLIED_THUNK:%[0-9]+]] = partial_apply [callee_guaranteed] [[OWNED_TO_GUARANTEED_THUNK]]([[THICK_SHARED_THUNK]]) : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate, @noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [[APPLIED_THUNK]]
  // CHECK: [[RECUR_FN:%[0-9]+]] = function_ref @$s6shared09owned_to_A11_conversionyyySi_AA14ValueAggregateVAA03RefF0CtXEF : $@convention(thin) (@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
  // CHECK: {{%.*}} = apply [[RECUR_FN]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
  // CHECK-NOT: destroy_value [[UNUSED_FUNC]]
  // CHECK: } // end sil function '$s6shared09owned_to_A11_conversionyyySi_AA14ValueAggregateVAA03RefF0CtXEF'

  // ======== REABSTRACTION THUNK =========

  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sSi6shared14ValueAggregateVAA03RefC0CIgygg_SiAcEIegyxx_TR : $@convention(thin) (Int, @owned ValueAggregate, @owned RefAggregate, @noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()) -> ()
  // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @owned $ValueAggregate, [[REF_VAL:%[0-9]+]] : @owned $RefAggregate, [[FUNC:%[0-9]+]] : $@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()):
  // CHECK: [[BORROW_VALUE_VAL:%[0-9]+]] = begin_borrow [[VALUE_VAL]] : $ValueAggregate
  // CHECK: [[BORROW_REF_VAL:%[0-9]+]] = begin_borrow [[REF_VAL]] : $RefAggregate
  // CHECK: {{%.*}} = apply [[FUNC]]([[TRIVIAL_VAL]], [[BORROW_VALUE_VAL]], [[BORROW_REF_VAL]]) : $@noescape @callee_guaranteed (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate) -> ()
  // CHECK: end_borrow [[BORROW_REF_VAL]] : $RefAggregate
  // CHECK: end_borrow [[BORROW_VALUE_VAL]] : $ValueAggregate
  // CHECK: destroy_value [[REF_VAL]] : $RefAggregate
  // CHECK: destroy_value [[VALUE_VAL]] : $ValueAggregate
  // CHECK: } // end sil function '$sSi6shared14ValueAggregateVAA03RefC0CIgygg_SiAcEIegyxx_TR'
  
  return owned_to_shared_conversion { (trivial : __shared Int, val : __shared ValueAggregate, ref : __shared RefAggregate) in }
}

// CHECK-LABEL: sil hidden @$s6shared0A17_closure_loweringyyySi_AA14ValueAggregateVAA03RefE0CtchF : $@convention(thin) (@guaranteed @callee_guaranteed (Int, @owned ValueAggregate, @owned RefAggregate) -> ()) -> ()
func shared_closure_lowering(_ f : __shared (Int, ValueAggregate, RefAggregate) -> Void) {}

struct Foo {
    var x: ValueAggregate

    // CHECK-LABEL: sil hidden @$s6shared3FooV21methodSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtF : $@convention(method) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate, @guaranteed Foo) -> () {
    func methodSharedArguments(trivial : __shared Int, value : __shared ValueAggregate, ref : __shared RefAggregate) {
        // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate, [[SELF:%[0-9]+]] : @guaranteed $Foo):
        // CHECK: [[COPY_VALUE_VAL:%[0-9]+]] = copy_value [[VALUE_VAL]] : $ValueAggregate
        // CHECK: [[COPY_REF_VAL:%[0-9]+]] = copy_value [[REF_VAL]] : $RefAggregate
        // CHECK: [[OWNED_FUNC:%[0-9]+]] = function_ref @$s6shared3FooV20methodOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtF
        // CHECK: {{%.*}} = apply [[OWNED_FUNC]]([[TRIVIAL_VAL]], [[COPY_VALUE_VAL]], [[COPY_REF_VAL]], [[SELF]]) : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @guaranteed Foo) -> ()
        // CHECK: } // end sil function '$s6shared3FooV21methodSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtF'
        return methodOwnedArguments(trivial: trivial, value: value, ref: ref)
    }

    // CHECK-LABEL: sil hidden @$s6shared3FooV20methodOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtF : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @guaranteed Foo) -> () {
    func methodOwnedArguments(trivial : Int, value : ValueAggregate, ref : RefAggregate) {
        // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @owned $ValueAggregate, [[REF_VAL:%[0-9]+]] : @owned $RefAggregate, [[SELF:%[0-9]+]] : @guaranteed $Foo):
        // CHECK: [[BORROW_VALUE_VAL:%[0-9]+]] = begin_borrow [[VALUE_VAL]] : $ValueAggregate
        // CHECK: [[BORROW_REF_VAL:%[0-9]+]] = begin_borrow [[REF_VAL]] : $RefAggregate
        // CHECK: [[SHARED_FUNC:%[0-9]+]] = function_ref @$s6shared3FooV21methodSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtF
        // CHECK: {{%.*}} = apply [[SHARED_FUNC]]([[TRIVIAL_VAL]], [[BORROW_VALUE_VAL]], [[BORROW_REF_VAL]], [[SELF]])
        // CHECK: end_borrow [[BORROW_REF_VAL]] : $RefAggregate
        // CHECK: end_borrow [[BORROW_VALUE_VAL]] : $ValueAggregate
        // CHECK: destroy_value [[REF_VAL]] : $RefAggregate
        // CHECK: destroy_value [[VALUE_VAL]] : $ValueAggregate
        // CHECK: } // end sil function '$s6shared3FooV20methodOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtF'
        return methodSharedArguments(trivial: trivial, value: value, ref: ref)
    }

    // CHECK-LABEL: sil hidden @$s6shared3FooV21staticSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtFZ : $@convention(method) (Int, @guaranteed ValueAggregate, @guaranteed RefAggregate, @thin Foo.Type) -> () {
    static func staticSharedArguments(trivial : __shared Int, value : __shared ValueAggregate, ref : __shared RefAggregate) {
        // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @guaranteed $ValueAggregate, [[REF_VAL:%[0-9]+]] : @guaranteed $RefAggregate, [[SELF_METATYPE:%[0-9]+]] : $@thin Foo.Type):
        // CHECK: [[COPY_VALUE_VAL:%[0-9]+]] = copy_value [[VALUE_VAL]] : $ValueAggregate
        // CHECK: [[COPY_REF_VAL:%[0-9]+]] = copy_value [[REF_VAL]] : $RefAggregate
        // CHECK: [[OWNED_FUNC:%[0-9]+]] = function_ref @$s6shared3FooV20staticOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtFZ
        // CHECK: {{%.*}} = apply [[OWNED_FUNC]]([[TRIVIAL_VAL]], [[COPY_VALUE_VAL]], [[COPY_REF_VAL]], [[SELF_METATYPE]]) : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @thin Foo.Type) -> ()
        // CHECK: } // end sil function '$s6shared3FooV21staticSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtFZ'
        return staticOwnedArguments(trivial: trivial, value: value, ref: ref)
    }
    // CHECK-LABEL: sil hidden @$s6shared3FooV20staticOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtFZ : $@convention(method) (Int, @owned ValueAggregate, @owned RefAggregate, @thin Foo.Type) -> () {
    static func staticOwnedArguments(trivial : Int, value : ValueAggregate, ref : RefAggregate) {
        // CHECK: bb0([[TRIVIAL_VAL:%[0-9]+]] : $Int, [[VALUE_VAL:%[0-9]+]] : @owned $ValueAggregate, [[REF_VAL:%[0-9]+]] : @owned $RefAggregate, [[SELF_METATYPE:%[0-9]+]] : $@thin Foo.Type):
        // CHECK: [[BORROW_VALUE_VAL:%[0-9]+]] = begin_borrow [[VALUE_VAL]] : $ValueAggregate
        // CHECK: [[BORROW_REF_VAL:%[0-9]+]] = begin_borrow [[REF_VAL]] : $RefAggregate
        // CHECK: [[SHARED_FUNC:%[0-9]+]] = function_ref @$s6shared3FooV21staticSharedArguments7trivial5value3refySih_AA14ValueAggregateVhAA03RefJ0ChtFZ
        // CHECK: {{%.*}} = apply [[SHARED_FUNC]]([[TRIVIAL_VAL]], [[BORROW_VALUE_VAL]], [[BORROW_REF_VAL]], [[SELF_METATYPE]])
        // CHECK: end_borrow [[BORROW_REF_VAL]] : $RefAggregate
        // CHECK: end_borrow [[BORROW_VALUE_VAL]] : $ValueAggregate
        // CHECK: destroy_value [[REF_VAL]] : $RefAggregate
        // CHECK: destroy_value [[VALUE_VAL]] : $ValueAggregate
        // CHECK: } // end sil function '$s6shared3FooV20staticOwnedArguments7trivial5value3refySi_AA14ValueAggregateVAA03RefJ0CtFZ'
        return staticSharedArguments(trivial: trivial, value: value, ref: ref)
    }
}
