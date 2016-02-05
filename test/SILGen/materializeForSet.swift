// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

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
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[ADDR]]
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet4Basem8computedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> ()
// CHECK: [[T2:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type)
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T2]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>)
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_TFFC17materializeForSet4Basem8computedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Base, [[SELFTYPE:%.*]] : $@thick Base.Type):
// CHECK:   [[T0:%.*]] = load [[SELF]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T2:%.*]] = load [[T1]] : $*Int
// CHECK:   [[SETTER:%.*]] = function_ref @_TFC17materializeForSet4Bases8computedSi
// CHECK:   apply [[SETTER]]([[T2]], [[T0]])

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet4Basem6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T2:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Base, @thick Base.Type) -> ()>, #Optional.None
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
// CHECK: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m14storedFunction
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned () -> @out Int
// CHECK-NEXT: [[T0:%.*]] = load %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $@callee_owned () -> Int
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!getter.1
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]([[SELF]])
// CHECK-NEXT: store [[RESULT]] to [[TEMP]]
// CHECK-NEXT: [[RESULT:%.*]] = load [[TEMP]]
// CHECK-NEXT: strong_retain [[RESULT]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: store [[T1]] to [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem14storedFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>)
// CHECK-NEXT: destroy_addr [[TEMP]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NEXT: return [[T0]]

// CHECK: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem14storedFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Derived, @thick Derived.Type) -> ()
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!setter.1 : (Base) -> (() -> Int) -> ()
// CHECK-NEXT: apply [[FN]]([[NEWVALUE]], [[SELF]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m19finalStoredFunction
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned () -> @out Int
// CHECK-NEXT: [[T0:%.*]] = load %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: [[RESULT:%.*]] = load [[ADDR]]
// CHECK-NEXT: strong_retain [[RESULT]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: store [[T1]] to [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem19finalStoredFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>, #Optional.Some!enumelt.1, [[T0]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Derived, @thick Derived.Type) -> ()>)
// CHECK-NEXT: return [[T0]]

// CHECK: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFCS_4Basem19finalStoredFunctionFT_SiU_XfTBpRBBRS2_XMTS2__T_ :
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: assign [[NEWVALUE]] to [[ADDR]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

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

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet9HasDidSetm6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg6storedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm6storedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>, #Optional.Some!enumelt.1, [[CALLBACK_FN]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK: }

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
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm8computedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>, #Optional.Some!enumelt.1, [[CALLBACK_FN]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasDidSet, @thick HasDidSet.Type) -> ()>)
// CHECK: }
}

class HasStoredDidSet {
  var stored: Int = 0 {
    didSet {}
  }

// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet15HasStoredDidSetm6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasStoredDidSet) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasStoredDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet15HasStoredDidSetg6storedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet15HasStoredDidSetm6storedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()>, #Optional.Some!enumelt.1, [[CALLBACK_FN]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()>)
// CHECK: }
}

// CHECK-LABEL: sil hidden [transparent] @_TFFC17materializeForSet15HasStoredDidSetm6storedSiU_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*HasStoredDidSet, [[METATYPE:%.*]] : $@thick HasStoredDidSet.Type):
// CHECK:   [[SELF_VALUE:%.*]] = load [[SELF]] : $*HasStoredDidSet
// CHECK:   [[BUFFER_ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Int
// CHECK:   [[VALUE:%.*]] = load [[BUFFER_ADDR]] : $*Int
// CHECK:   [[SETTER_FN:%.*]] = function_ref @_TFC17materializeForSet15HasStoredDidSets6storedSi : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   apply [[SETTER_FN]]([[VALUE]], [[SELF_VALUE]]) : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   return
// CHECK: }

class HasWeak {
  weak var weakvar: HasWeak? = nil
}
// CHECK-LABEL: sil hidden [transparent] @_TFC17materializeForSet7HasWeakm7weakvarXwGSqS0__ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasWeak) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasWeak, @thick HasWeak.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasWeak):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to $*Optional<HasWeak>
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[T1:%.*]] = load_weak [[T0]] : $*@sil_weak Optional<HasWeak>
// CHECK:   store [[T1]] to [[T2]] : $*Optional<HasWeak>
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet7HasWeakm7weakvarXwGSqS0__U_XfTBpRBBRS0_XMTS0__T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasWeak, @thick HasWeak.Type) -> () 
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
// CHECK-LABEL: sil hidden @_TF17materializeForSet13improveWizardFRVS_6WizardT_
// CHECK:       [[IMPROVE:%.*]] = function_ref @_TF17materializeForSet7improveFRSiT_ :
// CHECK-NEXT:  [[TEMP:%.*]] = alloc_stack $Int
//   Call the getter and materialize the result in the temporary.
// CHECK-NEXT:  [[T0:%.*]] = load [[WIZARD:.*]] : $*Wizard
// CHECK-NEXT:  function_ref
// CHECK-NEXT:  [[GETTER:%.*]] = function_ref @_TFE17materializeForSetPS_5Magicg5hocusSi
// CHECK-NEXT:  [[WTEMP:%.*]] = alloc_stack $Wizard
// CHECK-NEXT:  store [[T0]] to [[WTEMP]]
// CHECK-NEXT:  [[T0:%.*]] = apply [[GETTER]]<Wizard>([[WTEMP]])
// CHECK-NEXT:  store [[T0]] to [[TEMP]]
//   Call improve.
// CHECK-NEXT:  apply [[IMPROVE]]([[TEMP]])
// CHECK-NEXT:  [[T0:%.*]] = load [[TEMP]]
// CHECK-NEXT:  function_ref
// CHECK-NEXT:  [[SETTER:%.*]] = function_ref @_TFE17materializeForSetPS_5Magics5hocusSi
// CHECK-NEXT:  apply [[SETTER]]<Wizard>([[T0]], [[WIZARD]])
// CHECK-NEXT:  dealloc_stack [[WTEMP]]
// CHECK-NEXT:  dealloc_stack [[TEMP]]

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// CHECK-LABEL: sil hidden [transparent] @_TFV17materializeForSet4Billm5totalSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*Bill, #Bill.total
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T3:%.*]] = enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>, #Optional.None!enumelt
// CHECK:   [[T4:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T3]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>)
// CHECK: }

// CHECK-LABEL:  sil hidden [transparent] [thunk] @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Bill, @thick Bill.Type) -> ()>) {
// CHECK:  bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:    [[T0:%.*]] = function_ref @_TFV17materializeForSet4Billm5totalSi
// CHECK:    [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[STORAGE]], [[SELF]])
// CHECK:    return [[T1]] :

// CHECK: sil_witness_table hidden Bill: Totalled module materializeForSet {
// CHECK:   method #Totalled.total!getter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_g5totalSi
// CHECK:   method #Totalled.total!setter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_s5totalSi
// CHECK:   method #Totalled.total!materializeForSet.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi
// CHECK: }

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

// Test for materializeForSet vs static properties of structs.

protocol Beverage {
  static var abv: Int { get set }
}

struct Beer : Beverage {
  static var abv: Int {
    get {
      return 7
    }
    set { }
  }
}

struct Wine<Color> : Beverage {
  static var abv: Int {
    get {
      return 14
    }
    set { }
  }
}
