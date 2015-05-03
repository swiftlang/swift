// RUN: %target-swift-frontend -emit-sil -parse-stdlib %s | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -parse-stdlib %s | FileCheck %s -check-prefix=SILGEN

import Swift

func test_callback() {
  let callback = Builtin.makeMaterializeForSetCallback
    {(value, storage, inout selfV: Int, type) -> () in ()}
}
// CHECK: sil hidden @_TF17materializeForSet13test_callbackFT_T_ : $@convention(thin) () -> ()
// CHECK:   [[T0:%.*]] = function_ref @_TFF17materializeForSet13test_callbackFT_T_U_FTBpRBBRSiMSi_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Int, @thick Int.Type) -> ()

// CHECK: sil shared @_TFF17materializeForSet13test_callbackFT_T_U_FTBpRBBRSiMSi_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Int, @thick Int.Type) -> () {
// CHECK:  bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Int, %3 : $@thick Int.Type):
// CHECK-NOT: alloc_box $Builtin.UnsafeValueBuffer
// CHECK:    [[T0:%.*]] = metatype $@thin Int.Type
// CHECK:    debug_value [[T0]] : $@thin Int.Type

class Base {
  var stored: Int = 0

// The ordering here is unfortunate: we generate the property
// getters and setters after we've processed the decl.

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet4Basem8computedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet4Baseg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [[ADDR]] : $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet4Basem8computedSiU_FTBpRBBRS0_MS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> ()
// CHECK: [[T2:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type)
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T2]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK: }

// CHECK-LABEL: sil @_TFFC17materializeForSet4Basem8computedSiU_FTBpRBBRS0_MS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Base, [[SELFTYPE:%.*]] : $@thick Base.Type):
// CHECK:   [[T0:%.*]] = load [[SELF]]
// CHECK:   strong_retain [[T0]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = load [[T1]] : $*Int
// CHECK:   [[SETTER:%.*]] = function_ref @_TFC17materializeForSet4Bases8computedSi
// CHECK:   apply [[SETTER]]([[T2]], [[T0]])

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet4Basem6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   inject_enum_addr [[TMP:%.*]]#1 : $*Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>, #Optional.None
// CHECK:   [[T2:%.*]] = load [[TMP]]#1
// CHECK:   [[T3:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T2]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK:   return [[T3]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK: }

  var computed: Int {
    get { return 0 }
    set(value) {}
  }

}

class HasDidSet : Base {
  override var stored: Int {
    didSet {}
  }

// Checking this after silgen, but before mandatory inlining, lets us
// test the intent much better.

// SILGEN-LABEL: sil hidden [transparent] @_TFC17materializeForSet9HasDidSetm6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>) {
// SILGEN: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// SILGEN:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// SILGEN:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg6storedSi
// SILGEN:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// SILGEN:   store [[T1]] to [[T2]] : $*Int
// SILGEN:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm8computedSiU_FTBpRBBRS0_MS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// SILGEN:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, {{.*}} : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// SILGEN:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// SILGEN: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet9HasDidSetm8computedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm8computedSiU_FTBpRBBRS0_MS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, {{.*}} : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK: }
}

class HasWeak {
  weak var weakvar: HasWeak? = nil
}
// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet7HasWeakm7weakvarXwGSqS0__ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasWeak) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasWeak, @thick HasWeak.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasWeak):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Optional<HasWeak>
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[T1:%.*]] = load_weak [[T0]] : $*@sil_weak Optional<HasWeak>
// CHECK:   store [[T1]] to [[T2]] : $*Optional<HasWeak>
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet7HasWeakm7weakvarXwGSqS0__U_FTBpRBBRS0_MS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasWeak, @thick HasWeak.Type) -> () 
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, {{.*}} : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasWeak, @thick HasWeak.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasWeak, @thick HasWeak.Type) -> ()>)
// CHECK: }

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// SILGEN-LABEL: sil hidden [transparent] @_TFV17materializeForSet4Billm5totalSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>) {
// SILGEN: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// SILGEN:   debug_value %0 : $Builtin.RawPointer
// SILGEN:   [[BOX:%.*]] = alloc_box $Bill
// SILGEN:   copy_addr [[SELF]] to [initialization] [[BOX]]#1
// SILGEN:   [[T0:%.*]] = struct_element_addr [[BOX]]#1 : $*Bill, #Bill.total
// SILGEN:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// SILGEN:   [[INIT:%.*]] = function_ref @_TFSqCU__fMGSqQ__FT10nilLiteralT__GSqQ__ : $@convention(thin) <τ_0_0> (@out Optional<τ_0_0>, @thin Optional<τ_0_0>.Type) -> ()
// SILGEN:   [[META:%.*]] = metatype $@thin Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>.Type
// SILGEN:   [[T2:%.*]] = alloc_stack $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>
// SILGEN:   [[OPT:%.*]] = apply [[INIT]]<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>([[T2]]#1, [[META]]) : $@convention(thin) <τ_0_0> (@out Optional<τ_0_0>, @thin Optional<τ_0_0>.Type) -> ()
// SILGEN:   [[T3:%.*]] = load [[T2]]#1
// SILGEN:   [[T4:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T3]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>)
// SILGEN:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>)
// SILGEN: }

// SILGEN-LABEL:  sil hidden [transparent] [thunk] @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>) {
// SILGEN:  bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// SILGEN:    [[T0:%.*]] = function_ref @_TFV17materializeForSet4Billm5totalSi
// SILGEN:    [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[STORAGE]], [[SELF]])
// SILGEN:    return [[T1]] :

// SILGEN: sil_witness_table hidden Bill: Totalled module materializeForSet {
// SILGEN:   method #Totalled.total!getter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_g5totalSi
// SILGEN:   method #Totalled.total!setter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_s5totalSi
// SILGEN:   method #Totalled.total!materializeForSet.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi
// SILGEN: }
