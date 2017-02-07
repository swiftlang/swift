// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class Base {
  var stored: Int = 0

// The ordering here is unfortunate: we generate the property
// getters and setters after we've processed the decl.

// CHECK-LABEL: sil hidden @_TFC17materializeForSet4Basem8computedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet4Baseg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[ADDR]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[ADDR]]
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet4Basem8computedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> ()
// CHECK:   [[T2:%.*]] = thin_function_to_pointer [[T0]]
// CHECK:   [[T3:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T2]] : $Builtin.RawPointer
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

// CHECK-LABEL: sil hidden @_TFFC17materializeForSet4Basem8computedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Base, [[SELFTYPE:%.*]] : $@thick Base.Type):
// CHECK:   [[T0:%.*]] = load_borrow [[SELF]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T2:%.*]] = load [trivial] [[T1]] : $*Int
// CHECK:   [[SETTER:%.*]] = function_ref @_TFC17materializeForSet4Bases8computedSi
// CHECK:   apply [[SETTER]]([[T2]], [[T0]])

// CHECK-LABEL: sil hidden @_TFC17materializeForSet4Basem6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T2:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.none
// CHECK:   [[T3:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T2]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T3]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
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
  static var staticFunction: () -> Int {
    get { return {0} }
    set {}
  }
}

class Derived : Base {}

protocol Abstractable {
  associatedtype Result
  var storedFunction: () -> Result { get set }
  var finalStoredFunction: () -> Result { get set }
  var computedFunction: () -> Result { get set }
  static var staticFunction: () -> Result { get set }
}

// Validate that we thunk materializeForSet correctly when there's
// an abstraction pattern present.

extension Derived : Abstractable {}
// CHECK: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m14storedFunction
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $@callee_owned () -> Int
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!getter.1
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]([[SELF]])
// CHECK-NEXT: store [[RESULT]] to [init] [[TEMP]]
// CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[TEMP]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: store [[T1]] to [init] [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFS1_m14storedFunctionFT_wx6ResultU_T_
// CHECK-NEXT: [[T1:%.*]] = thin_function_to_pointer [[T0]]
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: destroy_addr [[TEMP]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NEXT: return [[T0]]

// CHECK: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFS1_m14storedFunctionFT_wx6ResultU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Derived, @thick Derived.Type) -> ()
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!setter.1 : (Base) -> (@escaping () -> Int) -> ()
// CHECK-NEXT: apply [[FN]]([[NEWVALUE]], [[SELF]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FS1_m19finalStoredFunction
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[ADDR]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: store [[T1]] to [init] [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFS1_m19finalStoredFunctionFT_wx6ResultU_T_
// CHECK-NEXT: [[T1:%.*]] = thin_function_to_pointer [[T0]]
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: return [[T0]]

// CHECK: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FFS1_m19finalStoredFunctionFT_wx6ResultU_T_ :
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: assign [[NEWVALUE]] to [[ADDR]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC17materializeForSet7DerivedS_12AbstractableS_ZFS1_m14staticFunctionFT_wx6Result
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $@thick Derived.Type):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[SELF:%.*]] = upcast %2 : $@thick Derived.Type to $@thick Base.Type
// CHECK-NEXT: [[OUT:%.*]] = alloc_stack $@callee_owned () -> Int
// CHECK:      [[GETTER:%.*]] = function_ref @_TZFC17materializeForSet4Baseg14staticFunctionFT_Si : $@convention(method) (@thick Base.Type) -> @owned @callee_owned () -> Int
// CHECK-NEXT: [[VALUE:%.*]] = apply [[GETTER]]([[SELF]]) : $@convention(method) (@thick Base.Type) -> @owned @callee_owned () -> Int
// CHECK-NEXT: store [[VALUE]] to [init] [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: [[VALUE:%.*]] = load [copy] [[OUT]] : $*@callee_owned () -> Int
// CHECK:      [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__dSi_XFo__iSi_ : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: store [[NEWVALUE]] to [init] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: [[ADDR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK:      [[CALLBACK_FN:%.*]] = function_ref @_TTWC17materializeForSet7DerivedS_12AbstractableS_FZFS1_m14staticFunctionFT_wx6ResultU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout @thick Derived.Type, @thick Derived.Type.Type) -> ()
// CHECK-NEXT: [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout @thick Derived.Type, @thick Derived.Type.Type) -> () to $Builtin.RawPointer
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]] : $Builtin.RawPointer
// CHECK-NEXT: [[RESULT:%.*]] = tuple ([[ADDR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: destroy_addr [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: dealloc_stack [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: return [[RESULT]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)

// CHECK-LABEL: sil hidden [transparent] @_TTWC17materializeForSet7DerivedS_12AbstractableS_FZFS1_m14staticFunctionFT_wx6ResultU_T_
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*@thick Derived.Type, %3 : $@thick Derived.Type.Type):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow %2 : $*@thick Derived.Type
// CHECK-NEXT: [[BASE_SELF:%.*]] = upcast [[SELF]] : $@thick Derived.Type to $@thick Base.Type
// CHECK-NEXT: [[BUFFER:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[BUFFER]] : $*@callee_owned () -> @out Int
// CHECK:      [[REABSTRACTOR:%.*]] = function_ref @_TTRXFo__iSi_XFo__dSi_ : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]]) : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK:      [[SETTER_FN:%.*]] = function_ref @_TZFC17materializeForSet4Bases14staticFunctionFT_Si : $@convention(method) (@owned @callee_owned () -> Int, @thick Base.Type) -> ()
// CHECK-NEXT: apply [[SETTER_FN]]([[NEWVALUE]], [[BASE_SELF]]) : $@convention(method) (@owned @callee_owned () -> Int, @thick Base.Type) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

protocol ClassAbstractable : class {
  associatedtype Result
  var storedFunction: () -> Result { get set }
  var finalStoredFunction: () -> Result { get set }
  var computedFunction: () -> Result { get set }
  static var staticFunction: () -> Result { get set }
}

extension Derived : ClassAbstractable {}

protocol Signatures {
  associatedtype Result
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

// CHECK-LABEL: sil hidden @_TFC17materializeForSet9HasDidSetm6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg6storedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm6storedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK-LABEL: sil hidden @_TFC17materializeForSet9HasDidSetm8computedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet9HasDidSetg8computedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet9HasDidSetm8computedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }
}

class HasStoredDidSet {
  var stored: Int = 0 {
    didSet {}
  }

// CHECK-LABEL: sil hidden @_TFC17materializeForSet15HasStoredDidSetm6storedSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasStoredDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasStoredDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_TFC17materializeForSet15HasStoredDidSetg6storedSi
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_TFFC17materializeForSet15HasStoredDidSetm6storedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }
}

// CHECK-LABEL: sil hidden @_TFFC17materializeForSet15HasStoredDidSetm6storedSiU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*HasStoredDidSet, [[METATYPE:%.*]] : $@thick HasStoredDidSet.Type):
// CHECK:   [[SELF_VALUE:%.*]] = load_borrow [[SELF]] : $*HasStoredDidSet
// CHECK:   [[BUFFER_ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[VALUE:%.*]] = load [trivial] [[BUFFER_ADDR]] : $*Int
// CHECK:   [[SETTER_FN:%.*]] = function_ref @_TFC17materializeForSet15HasStoredDidSets6storedSi : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   apply [[SETTER_FN]]([[VALUE]], [[SELF_VALUE]]) : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   return
// CHECK: }

class HasWeak {
  weak var weakvar: HasWeak?
}
// CHECK-LABEL: sil hidden @_TFC17materializeForSet7HasWeakm7weakvarXwGSqS0__ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasWeak) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasWeak):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Optional<HasWeak>
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[T1:%.*]] = load_weak [[T0]] : $*@sil_weak Optional<HasWeak>
// CHECK:   store [[T1]] to [init] [[T2]] : $*Optional<HasWeak>
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[T0:%.*]] = function_ref @_TFFC17materializeForSet7HasWeakm7weakvarXwGSqS0__U_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasWeak, @thick HasWeak.Type) -> () 
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, {{.*}} : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
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
func improve(_ x: inout Int) {}
func improveWizard(_ wizard: inout Wizard) {
  improve(&wizard.hocus)
}
// CHECK-LABEL: sil hidden @_TF17materializeForSet13improveWizardFRVS_6WizardT_
// CHECK:       [[IMPROVE:%.*]] = function_ref @_TF17materializeForSet7improveFRSiT_ :
// CHECK-NEXT:  [[TEMP:%.*]] = alloc_stack $Int
//   Call the getter and materialize the result in the temporary.
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[WIZARD:.*]] : $*Wizard
// CHECK-NEXT:  function_ref
// CHECK-NEXT:  [[GETTER:%.*]] = function_ref @_TFE17materializeForSetPS_5Magicg5hocusSi
// CHECK-NEXT:  [[WTEMP:%.*]] = alloc_stack $Wizard
// CHECK-NEXT:  store [[T0]] to [trivial] [[WTEMP]]
// CHECK-NEXT:  [[T0:%.*]] = apply [[GETTER]]<Wizard>([[WTEMP]])
// CHECK-NEXT:  store [[T0]] to [trivial] [[TEMP]]
//   Call improve.
// CHECK-NEXT:  apply [[IMPROVE]]([[TEMP]])
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[TEMP]]
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

// CHECK-LABEL: sil hidden [transparent] @_TFV17materializeForSet4Billm5totalSi : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*Bill, #Bill.total
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T3:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.none!enumelt
// CHECK:   [[T4:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T3]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

// CHECK-LABEL:  sil hidden [transparent] [thunk] @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:  bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:    [[T0:%.*]] = function_ref @_TFV17materializeForSet4Billm5totalSi
// CHECK:    [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[STORAGE]], [[SELF]])
// CHECK:    return [[T1]] :

protocol AddressOnlySubscript {
  associatedtype Index
  subscript(i: Index) -> Index { get set }
}

struct Foo<T>: AddressOnlySubscript {
  subscript(i: T) -> T {
    get { return i }
    set { print("\(i) = \(newValue)") }
  }
}

func increment(_ x: inout Int) { x += 1 }

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

// Make sure we can perform an inout access of such a property too.

func inoutAccessOfStaticProperty<T : Beverage>(_ t: T.Type) {
  increment(&t.abv)
}

// Test for materializeForSet vs overriden computed property of classes.
class BaseForOverride {
  var valueStored: Int
  var valueComputed: Int { get { } set { } }

  init(valueStored: Int) {
    self.valueStored = valueStored
  }
}

class DerivedForOverride : BaseForOverride {
  override var valueStored: Int { get { } set { } }
  override var valueComputed: Int { get { } set { } }
}

// Test for materializeForSet vs static properties of classes.

class ReferenceBeer {
  class var abv: Int {
    get {
      return 7
    }
    set { }
  }
}

func inoutAccessOfClassProperty() {
  increment(&ReferenceBeer.abv)
}

// Test for materializeForSet when Self is re-abstracted.
//
// We have to open-code the materializeForSelf witness, and not screw up
// the re-abstraction.

protocol Panda {
  var x: (Self) -> Self { get set }
}

func id<T>(_ t: T) -> T { return t }

extension Panda {
  var x: (Self) -> Self {
    get { return id }
    set { }
  }
}

struct TuxedoPanda : Panda { }

// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV17materializeForSet11TuxedoPandaS_5PandaS_FS1_m1xFxx

// Call the getter:

  // CHECK: function_ref @_TFE17materializeForSetPS_5Pandag1xFxx : $@convention(method) <τ_0_0 where τ_0_0 : Panda> (@in_guaranteed τ_0_0) -> @owned @callee_owned (@in τ_0_0) -> @out τ_0_0

// Result of calling the getter is re-abstracted to the maximally substituted type
// by SILGenFunction::emitApply():

  // CHECK: function_ref @_TTRXFo_iV17materializeForSet11TuxedoPanda_iS0__XFo_dS0__dS0__ : $@convention(thin) (TuxedoPanda, @owned @callee_owned (@in TuxedoPanda) -> @out TuxedoPanda) -> TuxedoPanda

// ... then we re-abstract to the requirement signature:
// FIXME: Peephole this away with the previous one since there's actually no
// abstraction change in this case.

  // CHECK: function_ref @_TTRXFo_dV17materializeForSet11TuxedoPanda_dS0__XFo_iS0__iS0__ : $@convention(thin) (@in TuxedoPanda, @owned @callee_owned (TuxedoPanda) -> TuxedoPanda) -> @out TuxedoPanda

// The callback:

  // CHECK: function_ref @_TTWV17materializeForSet11TuxedoPandaS_5PandaS_FFS1_m1xFxxU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout TuxedoPanda, @thick TuxedoPanda.Type) -> ()

// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_TTWV17materializeForSet11TuxedoPandaS_5PandaS_FFS1_m1xFxxU_T_ : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout TuxedoPanda, @thick TuxedoPanda.Type) -> ()

  // FIXME: Useless re-abstractions

  // CHECK: function_ref @_TTRXFo_iV17materializeForSet11TuxedoPanda_iS0__XFo_dS0__dS0__ : $@convention(thin) (TuxedoPanda, @owned @callee_owned (@in TuxedoPanda) -> @out TuxedoPanda) -> TuxedoPanda

  // CHECK: function_ref @_TFE17materializeForSetPS_5Pandas1xFxx : $@convention(method) <τ_0_0 where τ_0_0 : Panda> (@owned @callee_owned (@in τ_0_0) -> @out τ_0_0, @inout τ_0_0) -> ()

  // CHECK: function_ref @_TTRXFo_dV17materializeForSet11TuxedoPanda_dS0__XFo_iS0__iS0__ : $@convention(thin) (@in TuxedoPanda, @owned @callee_owned (TuxedoPanda) -> TuxedoPanda) -> @out TuxedoPanda

// CHECK: }


// Test for materializeForSet vs lazy properties of structs.

struct LazyStructProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_TF17materializeForSet31inoutAccessOfLazyStructPropertyFT1lRVS_18LazyStructProperty_T_
// CHECK:   function_ref @_TFV17materializeForSet18LazyStructPropertyg3catSi
// CHECK:   function_ref @_TFV17materializeForSet18LazyStructPropertys3catSi
func inoutAccessOfLazyStructProperty(l: inout LazyStructProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of classes.

// CHECK-LABEL: sil hidden @_TFC17materializeForSet17LazyClassPropertym3catSi

class LazyClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_TF17materializeForSet30inoutAccessOfLazyClassPropertyFT1lRCS_17LazyClassProperty_T_
// CHECK:    class_method {{.*}} : $LazyClassProperty, #LazyClassProperty.cat!materializeForSet.1
func inoutAccessOfLazyClassProperty(l: inout LazyClassProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of final classes.

final class LazyFinalClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_TF17materializeForSet35inoutAccessOfLazyFinalClassPropertyFT1lRCS_22LazyFinalClassProperty_T_
// CHECK:    function_ref @_TFC17materializeForSet22LazyFinalClassPropertyg3catSi
// CHECK:    function_ref @_TFC17materializeForSet22LazyFinalClassPropertys3catSi
func inoutAccessOfLazyFinalClassProperty(l: inout LazyFinalClassProperty) {
  increment(&l.cat)
}

// Make sure the below doesn't crash SILGen
struct FooClosure {
 var computed: (((Int) -> Int) -> Int)? {
   get { return stored }
   set {}
 }
 var stored: (((Int) -> Int) -> Int)? = nil
}

// CHECK-LABEL: _TF17materializeForSet22testMaterializedSetterFT_T_
func testMaterializedSetter() {
  // CHECK: function_ref @_TFV17materializeForSet10FooClosureCfT_S0_
  var f = FooClosure()
  // CHECK: function_ref @_TFV17materializeForSet10FooClosureg8computedGSqFFSiSiSi_
  // CHECK: function_ref @_TFV17materializeForSet10FooClosures8computedGSqFFSiSiSi_
  f.computed = f.computed
}

// CHECK-LABEL: sil_vtable DerivedForOverride {
// CHECK:   #BaseForOverride.valueComputed!getter.1: _TFC17materializeForSet18DerivedForOverrideg13valueComputedSi
// CHECK:   #BaseForOverride.valueComputed!setter.1: _TFC17materializeForSet18DerivedForOverrides13valueComputedSi
// CHECK:   #BaseForOverride.valueComputed!materializeForSet.1: _TFC17materializeForSet18DerivedForOverridem13valueComputedSi
// CHECK:   #BaseForOverride.valueStored!getter.1: _TFC17materializeForSet18DerivedForOverrideg11valueStoredSi
// CHECK:   #BaseForOverride.valueStored!setter.1: _TFC17materializeForSet18DerivedForOverrides11valueStoredSi
// CHECK:   #BaseForOverride.valueStored!materializeForSet.1: _TFC17materializeForSet18DerivedForOverridem11valueStoredSi
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden Bill: Totalled module materializeForSet {
// CHECK:   method #Totalled.total!getter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_g5totalSi
// CHECK:   method #Totalled.total!setter.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_s5totalSi
// CHECK:   method #Totalled.total!materializeForSet.1: @_TTWV17materializeForSet4BillS_8TotalledS_FS1_m5totalSi
// CHECK: }
