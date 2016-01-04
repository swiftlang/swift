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

  var storedFunction: () -> Int = { 0 }
  final var finalStoredFunction: () -> Int = { 0 }
  var computedFunction: () -> Int {
    get { return {0} }
    set {}
  }
}

class Derived : Base {}

protocol Abstractable {
  typealias Result
  var storedFunction: () -> Result { get set }
  var finalStoredFunction: () -> Result { get set }
  var computedFunction: () -> Result { get set }
}

// Validate that we thunk materializeForSet correctly when there's
// an abstraction pattern present.

extension Derived : Abstractable {}
// SILGEN: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m14storedFunction
// SILGEN: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// SILGEN-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: [[T0:%.*]] = load %2 : $*Derived
// SILGEN-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// SILGEN-NEXT: [[TEMP:%.*]] = alloc_stack $@callee_owned () -> Int
// SILGEN-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!getter.1
// SILGEN-NEXT: [[RESULT:%.*]] = apply [[FN]]([[SELF]])
// SILGEN-NEXT: store [[RESULT]] to [[TEMP]]
// SILGEN-NEXT: [[RESULT:%.*]] = load [[TEMP]]
// SILGEN-NEXT: strong_retain [[RESULT]]
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@out Int, @owned @callee_owned () -> Int) -> ()
// SILGEN-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// SILGEN-NEXT: store [[T1]] to [[RESULT_ADDR]]
// SILGEN-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned (@out Int) -> () to $Builtin.RawPointer
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem14storedFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_
// SILGEN-NEXT: [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]]
// SILGEN-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>)
// SILGEN-NEXT: destroy_addr [[TEMP]]
// SILGEN-NEXT: dealloc_stack [[TEMP]]
// SILGEN-NEXT: return [[T0]]

// SILGEN: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem14storedFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Derived, @thick Derived.Type) -> ()
// SILGEN: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// SILGEN-NEXT: [[T0:%.*]] = load %2 : $*Derived
// SILGEN-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// SILGEN-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: [[VALUE:%.*]] = load [[RESULT_ADDR]] : $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned (@out Int) -> ()) -> Int
// SILGEN-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// SILGEN-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!setter.1 : (Base) -> (() -> Int) -> ()
// SILGEN-NEXT: apply [[FN]]([[NEWVALUE]], [[SELF]])
// SILGEN-NEXT: tuple ()
// SILGEN-NEXT: return

// SILGEN: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m19finalStoredFunction
// SILGEN: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// SILGEN-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: [[T0:%.*]] = load %2 : $*Derived
// SILGEN-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// SILGEN-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// SILGEN-NEXT: [[RESULT:%.*]] = load [[ADDR]]
// SILGEN-NEXT: strong_retain [[RESULT]]
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@out Int, @owned @callee_owned () -> Int) -> ()
// SILGEN-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// SILGEN-NEXT: store [[T1]] to [[RESULT_ADDR]]
// SILGEN-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned (@out Int) -> () to $Builtin.RawPointer
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem19finalStoredFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_
// SILGEN-NEXT: [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]]
// SILGEN-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>)
// SILGEN-NEXT: return [[T0]]

// SILGEN: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem19finalStoredFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_ :
// SILGEN: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// SILGEN-NEXT: [[T0:%.*]] = load %2 : $*Derived
// SILGEN-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// SILGEN-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: [[VALUE:%.*]] = load [[RESULT_ADDR]] : $*@callee_owned (@out Int) -> ()
// SILGEN-NEXT: function_ref
// SILGEN-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned (@out Int) -> ()) -> Int
// SILGEN-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// SILGEN-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// SILGEN-NEXT: assign [[NEWVALUE]] to [[ADDR]]
// SILGEN-NEXT: tuple ()
// SILGEN-NEXT: return

protocol ClassAbstractable : class {
  typealias Result
  var storedFunction: () -> Result { get set }
  var finalStoredFunction: () -> Result { get set }
  var computedFunction: () -> Result { get set }
}
extension Derived : ClassAbstractable {}

protocol Signatures {
  typealias Result
  var computedFunction: () -> Result { get set }
}
protocol Implementations {}
extension Implementations {
  var computedFunction: () -> Int {
    get { return {0} }
    set {}
  }
}

class ImplementingClass : Implementations, Signatures {}
struct ImplementingStruct : Implementations, Signatures {
  var ref: ImplementingClass?
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

// rdar://22109071
// Test that we don't use materializeForSet from a protocol extension.
protocol Magic {}
extension Magic {
  var hocus: Int {
    get { return 0 }
    set {}
  }
}
struct Wizard : Magic {}
func improve(inout x: Int) {}
func improveWizard(inout wizard: Wizard) {
  improve(&wizard.hocus)
}
// SILGEN-LABEL: sil hidden @_TF17materializeForSet13improveWizardFRVS_6WizardT_
// SILGEN:       [[IMPROVE:%.*]] = function_ref @_TF17materializeForSet7improveFRSiT_ :
// SILGEN-NEXT:  [[TEMP:%.*]] = alloc_stack $Int
//   Call the getter and materialize the result in the temporary.
// SILGEN-NEXT:  [[T0:%.*]] = load [[WIZARD:.*]] : $*Wizard
// SILGEN-NEXT:  function_ref
// SILGEN-NEXT:  [[GETTER:%.*]] = function_ref @_TFE17materializeForSetPS_5Magicg5hocusSi
// SILGEN-NEXT:  [[WTEMP:%.*]] = alloc_stack $Wizard
// SILGEN-NEXT:  store [[T0]] to [[WTEMP]]#1
// SILGEN-NEXT:  [[T0:%.*]] = apply [[GETTER]]<Wizard>([[WTEMP]]#1)
// SILGEN-NEXT:  store [[T0]] to [[TEMP]]#1
//   Call improve.
// SILGEN-NEXT:  apply [[IMPROVE]]([[TEMP]]#1)
// SILGEN-NEXT:  [[T0:%.*]] = load [[TEMP]]#1
// SILGEN-NEXT:  function_ref
// SILGEN-NEXT:  [[SETTER:%.*]] = function_ref @_TFE17materializeForSetPS_5Magics5hocusSi
// SILGEN-NEXT:  apply [[SETTER]]<Wizard>([[T0]], [[WIZARD]])
// SILGEN-NEXT:  dealloc_stack [[WTEMP]]#0
// SILGEN-NEXT:  dealloc_stack [[TEMP]]#0

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
// SILGEN:   [[INIT:%.*]] = function_ref @_TFSqC
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

protocol AddressOnlySubscript {
  typealias Index
  subscript(i: Index) -> Index { get set }
}

struct Foo<T>: AddressOnlySubscript {
  subscript(i: T) -> T {
    get { return i }
    set { print("\(i) = \(newValue)") }
  }
}

