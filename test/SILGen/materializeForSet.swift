// RUN: %swift -emit-sil %s | FileCheck %s
// RUN: %swift -emit-silgen %s | FileCheck %s -check-prefix=SILGEN

class Base {
  var stored: Int = 0

// The ordering here is unfortunate: we generate the property
// getters and setters after we've processed the decl.

// CHECK: sil hidden [transparent] @_TFC17materializeForSet4Basem8computedSi : $@cc(method) @thin (Builtin.RawPointer, @owned Base) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet4Baseg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

// CHECK: sil hidden [transparent] @_TFC17materializeForSet4Basem6storedSi : $@cc(method) @thin (Builtin.RawPointer, @owned Base) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T2:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:   [[T3:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T2]] : $Builtin.Int1)
// CHECK:   return [[T3]] : $(Builtin.RawPointer, Builtin.Int1)
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

// SILGEN: sil hidden [transparent] @_TFC17materializeForSet9HasDidSetm6storedSi : $@cc(method) @thin (Builtin.RawPointer, @owned HasDidSet) -> (Builtin.RawPointer, Builtin.Int1) {
// SILGEN: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $HasDidSet):
// SILGEN:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg6storedSi
// SILGEN:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// SILGEN:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// SILGEN:   store [[T1]] to [[T2]] : $*Int
// SILGEN:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// SILGEN:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// SILGEN:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// SILGEN: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK: sil hidden [transparent] @_TFC17materializeForSet9HasDidSetm8computedSi : $@cc(method) @thin (Builtin.RawPointer, @owned HasDidSet) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }
}

class HasWeak {
  weak var weakvar: HasWeak? = nil
}
// CHECK: sil hidden [transparent] @_TFC17materializeForSet7HasWeakm7weakvarXwGSqS0__ : $@cc(method) @thin (Builtin.RawPointer, @owned HasWeak) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $HasWeak):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[T1:%.*]] = load_weak [[T0]] : $*@sil_weak Optional<HasWeak>
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Optional<HasWeak>
// CHECK:   store [[T1]] to [[T2]] : $*Optional<HasWeak>
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// CHECK: sil hidden [transparent] @_TFV17materializeForSet4Billm5totalSi : $@cc(method) @thin (Builtin.RawPointer, @inout Bill) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $*Bill):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*Bill, #Bill.total
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T2:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:   [[T3:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T2]] : $Builtin.Int1)
// CHECK:   return [[T3]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

// CHECK:  sil hidden @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Bill) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK:  bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $*Bill):
// CHECK:    [[T0:%.*]] = function_ref @_TFV17materializeForSet4Billm5totalSi
// CHECK:    [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[SELF]])
// CHECK:    return [[T1]] :

// CHECK: sil_witness_table hidden Bill: Totalled module materializeForSet {
// CHECK:   method #Totalled.total!getter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_g5totalSi
// CHECK:   method #Totalled.total!setter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_s5totalSi
// CHECK:   method #Totalled.total!materializeForSet.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi
// CHECK: }
