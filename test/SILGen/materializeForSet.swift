// RUN: %swift -emit-sil %s | FileCheck %s

class Base {
  var stored: Int = 0

// The ordering here is unfortunate: we generate the property
// getters and setters after we've processed the decl.

// CHECK: sil [transparent] @_TFC17materializeForSet4Basem8computedSi : $@cc(method) @thin (Builtin.RawPointer, @owned Base) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = class_method [[SELF]] : $Base, #Base.computed!getter.1 : Base -> () -> Int
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

// CHECK: sil [transparent] @_TFC17materializeForSet4Basem6storedSi : $@cc(method) @thin (Builtin.RawPointer, @owned Base) -> (Builtin.RawPointer, Builtin.Int1) {
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

// CHECK: sil [transparent] @_TFC17materializeForSet9HasDidSetm6storedSi : $@cc(method) @thin (Builtin.RawPointer, @owned HasDidSet) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T0:%.*]] = class_method [[SELF]] : $HasDidSet, #HasDidSet.stored!getter.1 : HasDidSet -> () -> Int
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK: sil [transparent] @_TFC17materializeForSet9HasDidSetm8computedSi : $@cc(method) @thin (Builtin.RawPointer, @owned HasDidSet) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T0:%.*]] = class_method [[SELF]] : $HasDidSet, #HasDidSet.computed!getter.1 : HasDidSet -> () -> Int
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[T3:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Builtin.Int1)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }
}

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// CHECK: sil [transparent] @_TFV17materializeForSet4Billm5totalSi : $@cc(method) @thin (Builtin.RawPointer, @inout Bill) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $*Bill):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*Bill, #Bill.total
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T2:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:   [[T3:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T2]] : $Builtin.Int1)
// CHECK:   return [[T3]] : $(Builtin.RawPointer, Builtin.Int1)
// CHECK: }

// CHECK:  sil @_TTWV17materializeForSet4BillS_8TotalledFS1_m5totalSi : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Bill) -> (Builtin.RawPointer, Builtin.Int1) {
// CHECK:  bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[SELF:%.*]] : $*Bill):
// CHECK:    [[T0:%.*]] = function_ref @_TFV17materializeForSet4Billm5totalSi
// CHECK:    [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[SELF]])
// CHECK:    return [[T1]] :

// CHECK: sil_witness_table Bill: Totalled module materializeForSet {
// CHECK:   method #Totalled.total!getter.1: @_TTWV17materializeForSet4BillS_8TotalledFS1_g5totalSi
// CHECK:   method #Totalled.total!setter.1: @_TTWV17materializeForSet4BillS_8TotalledFS1_s5totalSi
// CHECK:   method #Totalled.total!materializeForSet.1: @_TTWV17materializeForSet4BillS_8TotalledFS1_m5totalSi
// CHECK: }
