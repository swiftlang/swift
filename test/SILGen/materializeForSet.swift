// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

class Base {
  var stored: Int = 0

// The ordering here is unfortunate: we generate the property
// getters and setters after we've processed the decl.

  // CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet4BaseC8computedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Base, [[SELFTYPE:%.*]] : $@thick Base.Type):
// CHECK:   [[T0:%.*]] = load_borrow [[SELF]]
// CHECK:   [[T1:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T2:%.*]] = load [trivial] [[T1]] : $*Int
// CHECK:   [[SETTER:%.*]] = function_ref @_T017materializeForSet4BaseC8computedSifs
// CHECK:   apply [[SETTER]]([[T2]], [[T0]])

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet4BaseC8computedSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $Base):
// CHECK:   [[ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet4BaseC8computedSifg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[ADDR]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[ADDR]]
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet4BaseC8computedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Base, @thick Base.Type) -> ()
// CHECK:   [[T2:%.*]] = thin_function_to_pointer [[T0]]
// CHECK:   [[T3:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T2]] : $Builtin.RawPointer
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[T3]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet4BaseC6storedSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed Base) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
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

// CHECK: sil hidden [transparent] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14storedFunction6ResultQzycfmytfU_TW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Derived, @thick Derived.Type) -> ()
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxr_SiIxd_TR : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!setter.1 : (Base) -> (@escaping () -> Int) -> ()
// CHECK-NEXT: apply [[FN]]([[NEWVALUE]], [[SELF]])
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK: sil hidden [transparent] [thunk] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14storedFunction{{[_0-9a-zA-Z]*}}fmTW
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
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxd_SiIxr_TR : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: destroy_addr [[TEMP]]
// CHECK-NEXT: store [[T1]] to [init] [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14storedFunction6ResultQzycfmytfU_TW
// CHECK-NEXT: [[T1:%.*]] = thin_function_to_pointer [[T0]]
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NEXT: return [[T0]]

// CHECK: sil hidden [transparent] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP19finalStoredFunction6ResultQzycfmytfU_TW :
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived, %3 : $@thick Derived.Type):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxr_SiIxd_TR : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: assign [[NEWVALUE]] to [[ADDR]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

// CHECK: sil hidden [transparent] [thunk] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP19finalStoredFunction{{[_0-9a-zA-Z]*}}fmTW
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*Derived):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[T0:%.*]] = load_borrow %2 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[ADDR:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.finalStoredFunction
// CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[ADDR]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxd_SiIxr_TR : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[REABSTRACTOR]]([[RESULT]])
// CHECK-NEXT: store [[T1]] to [init] [[RESULT_ADDR]]
// CHECK-NEXT: [[RESULT_PTR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[T0:%.*]] = function_ref @_T017materializeForSet7DerivedCAA12AbstractableAaaDP19finalStoredFunction6ResultQzycfmytfU_TW
// CHECK-NEXT: [[T1:%.*]] = thin_function_to_pointer [[T0]]
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[T1]]
// CHECK-NEXT: [[T0:%.*]] = tuple ([[RESULT_PTR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: return [[T0]]

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14staticFunction6ResultQzycfmZytfU_TW
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*@thick Derived.Type, %3 : $@thick Derived.Type.Type):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow %2 : $*@thick Derived.Type
// CHECK-NEXT: [[BASE_SELF:%.*]] = upcast [[SELF]] : $@thick Derived.Type to $@thick Base.Type
// CHECK-NEXT: [[BUFFER:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[VALUE:%.*]] = load [take] [[BUFFER]] : $*@callee_owned () -> @out Int
// CHECK:      [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxr_SiIxd_TR : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]]) : $@convention(thin) (@owned @callee_owned () -> @out Int) -> Int
// CHECK:      [[SETTER_FN:%.*]] = function_ref @_T017materializeForSet4BaseC14staticFunctionSiycfsZ : $@convention(method) (@owned @callee_owned () -> Int, @thick Base.Type) -> ()
// CHECK-NEXT: apply [[SETTER_FN]]([[NEWVALUE]], [[BASE_SELF]]) : $@convention(method) (@owned @callee_owned () -> Int, @thick Base.Type) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14staticFunction6ResultQzycfmZTW
// CHECK: bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $@thick Derived.Type):
// CHECK-NEXT: [[RESULT_ADDR:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*@callee_owned () -> @out Int
// CHECK-NEXT: [[SELF:%.*]] = upcast %2 : $@thick Derived.Type to $@thick Base.Type
// CHECK-NEXT: [[OUT:%.*]] = alloc_stack $@callee_owned () -> Int
// CHECK:      [[GETTER:%.*]] = function_ref @_T017materializeForSet4BaseC14staticFunctionSiycfgZ : $@convention(method) (@thick Base.Type) -> @owned @callee_owned () -> Int
// CHECK-NEXT: [[VALUE:%.*]] = apply [[GETTER]]([[SELF]]) : $@convention(method) (@thick Base.Type) -> @owned @callee_owned () -> Int
// CHECK-NEXT: store [[VALUE]] to [init] [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: [[VALUE:%.*]] = load [copy] [[OUT]] : $*@callee_owned () -> Int
// CHECK:      [[REABSTRACTOR:%.*]] = function_ref @_T0SiIxd_SiIxr_TR : $@convention(thin) (@owned @callee_owned () -> Int) -> @out Int
// CHECK-NEXT: [[NEWVALUE:%.*]] = partial_apply [[REABSTRACTOR]]([[VALUE]])
// CHECK-NEXT: destroy_addr [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: store [[NEWVALUE]] to [init] [[RESULT_ADDR]] : $*@callee_owned () -> @out Int
// CHECK-NEXT: [[ADDR:%.*]] = address_to_pointer [[RESULT_ADDR]] : $*@callee_owned () -> @out Int to $Builtin.RawPointer
// CHECK:      [[CALLBACK_FN:%.*]] = function_ref @_T017materializeForSet7DerivedCAA12AbstractableAaaDP14staticFunction6ResultQzycfmZytfU_TW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout @thick Derived.Type, @thick Derived.Type.Type) -> ()
// CHECK-NEXT: [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]] : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout @thick Derived.Type, @thick Derived.Type.Type) -> () to $Builtin.RawPointer
// CHECK-NEXT: [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]] : $Builtin.RawPointer
// CHECK-NEXT: [[RESULT:%.*]] = tuple ([[ADDR]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT: dealloc_stack [[OUT]] : $*@callee_owned () -> Int
// CHECK-NEXT: return [[RESULT]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)

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

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet06HasDidC0C6storedSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet06HasDidC0C6storedSifg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_T017materializeForSet06HasDidC0C6storedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet06HasDidC0C8computedSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet06HasDidC0C8computedSifg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_T017materializeForSet06HasDidC0C8computedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasDidSet, @thick HasDidSet.Type) -> ()
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

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet012HasStoredDidC0C6storedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> () {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*HasStoredDidSet, [[METATYPE:%.*]] : $@thick HasStoredDidSet.Type):
// CHECK:   [[SELF_VALUE:%.*]] = load_borrow [[SELF]] : $*HasStoredDidSet
// CHECK:   [[BUFFER_ADDR:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[VALUE:%.*]] = load [trivial] [[BUFFER_ADDR]] : $*Int
// CHECK:   [[SETTER_FN:%.*]] = function_ref @_T017materializeForSet012HasStoredDidC0C6storedSifs : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   apply [[SETTER_FN]]([[VALUE]], [[SELF_VALUE]]) : $@convention(method) (Int, @guaranteed HasStoredDidSet) -> ()
// CHECK:   return
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet012HasStoredDidC0C6storedSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasStoredDidSet) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasStoredDidSet):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet012HasStoredDidC0C6storedSifg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[T2]] : $*Int
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[CALLBACK_FN:%.*]] = function_ref @_T017materializeForSet012HasStoredDidC0C6storedSifmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasStoredDidSet, @thick HasStoredDidSet.Type) -> ()
// CHECK:   [[CALLBACK_ADDR:%.*]] = thin_function_to_pointer [[CALLBACK_FN]]
// CHECK:   [[CALLBACK:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_ADDR]]
// CHECK:   [[T4:%.*]] = tuple ([[BUFFER]] : $Builtin.RawPointer, [[CALLBACK]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }
}

class HasWeak {
  weak var weakvar: HasWeak?
}
// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet7HasWeakC7weakvarACSgXwfm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasWeak) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $HasWeak):
// CHECK:   [[T2:%.*]] = pointer_to_address [[BUFFER]] : $Builtin.RawPointer to [strict] $*Optional<HasWeak>
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[T1:%.*]] = load_weak [[T0]] : $*@sil_weak Optional<HasWeak>
// CHECK:   store [[T1]] to [init] [[T2]] : $*Optional<HasWeak>
// CHECK:   [[BUFFER:%.*]] = address_to_pointer [[T2]]
// CHECK:   [[T0:%.*]] = function_ref @_T017materializeForSet7HasWeakC7weakvarACSgXwfmytfU_ : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasWeak, @thick HasWeak.Type) -> () 
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
// CHECK-LABEL: sil hidden @_T017materializeForSet13improveWizardyAA0E0VzF
// CHECK:       [[IMPROVE:%.*]] = function_ref @_T017materializeForSet7improveySizF :
// CHECK-NEXT:  [[TEMP:%.*]] = alloc_stack $Int
//   Call the getter and materialize the result in the temporary.
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[WIZARD:.*]] : $*Wizard
// CHECK-NEXT:  function_ref
// CHECK-NEXT:  [[GETTER:%.*]] = function_ref @_T017materializeForSet5MagicPAAE5hocusSifg
// CHECK-NEXT:  [[WTEMP:%.*]] = alloc_stack $Wizard
// CHECK-NEXT:  store [[T0]] to [trivial] [[WTEMP]]
// CHECK-NEXT:  [[T0:%.*]] = apply [[GETTER]]<Wizard>([[WTEMP]])
// CHECK-NEXT:  store [[T0]] to [trivial] [[TEMP]]
//   Call improve.
// CHECK-NEXT:  apply [[IMPROVE]]([[TEMP]])
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[TEMP]]
// CHECK-NEXT:  function_ref
// CHECK-NEXT:  [[SETTER:%.*]] = function_ref @_T017materializeForSet5MagicPAAE5hocusSifs
// CHECK-NEXT:  apply [[SETTER]]<Wizard>([[T0]], [[WIZARD]])
// CHECK-NEXT:  dealloc_stack [[WTEMP]]
// CHECK-NEXT:  dealloc_stack [[TEMP]]

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet4BillV5totalSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK: bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:   [[T0:%.*]] = struct_element_addr [[SELF]] : $*Bill, #Bill.total
// CHECK:   [[T1:%.*]] = address_to_pointer [[T0]] : $*Int to $Builtin.RawPointer
// CHECK:   [[T3:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.none!enumelt
// CHECK:   [[T4:%.*]] = tuple ([[T1]] : $Builtin.RawPointer, [[T3]] : $Optional<Builtin.RawPointer>)
// CHECK:   return [[T4]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK: }

// CHECK-LABEL:  sil hidden [transparent] [thunk] @_T017materializeForSet4BillVAA8TotalledAaaDP5totalSifmTW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Bill) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:        bb0([[BUFFER:%.*]] : $Builtin.RawPointer, [[STORAGE:%.*]] : $*Builtin.UnsafeValueBuffer, [[SELF:%.*]] : $*Bill):
// CHECK:          [[T0:%.*]] = function_ref @_T017materializeForSet4BillV5totalSifm
// CHECK-NEXT:     [[T1:%.*]] = apply [[T0]]([[BUFFER]], [[STORAGE]], [[SELF]])
// CHECK-NEXT:     [[LEFT:%.*]] = tuple_extract [[T1]]
// CHECK-NEXT:     [[RIGHT:%.*]] = tuple_extract [[T1]]
// CHECK-NEXT:     [[T1:%.*]] = tuple ([[LEFT]] : $Builtin.RawPointer, [[RIGHT]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT:     return [[T1]] :

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

// Generic subscripts.

protocol GenericSubscriptProtocol {
  subscript<T>(_: T) -> T { get set }
}

struct GenericSubscriptWitness : GenericSubscriptProtocol {
  subscript<T>(_: T) -> T { get { } set { } }
}

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet23GenericSubscriptWitnessV9subscriptxxclufmytfU_ : $@convention(method) <T> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout GenericSubscriptWitness, @thick GenericSubscriptWitness.Type) -> () {
// CHECK:       bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*GenericSubscriptWitness, %3 : $@thick GenericSubscriptWitness.Type):
// CHECK:         [[BUFFER:%.*]] = project_value_buffer $T in %1 : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:    [[INDICES:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*T
// CHECK:         [[SETTER:%.*]] = function_ref @_T017materializeForSet23GenericSubscriptWitnessV9subscriptxxclufs : $@convention(method) <τ_0_0> (@in τ_0_0, @in τ_0_0, @inout GenericSubscriptWitness) -> ()
// CHECK-NEXT:    apply [[SETTER]]<T>([[INDICES]], [[BUFFER]], %2) : $@convention(method) <τ_0_0> (@in τ_0_0, @in τ_0_0, @inout GenericSubscriptWitness) -> ()
// CHECK-NEXT:    dealloc_value_buffer $*T in %1 : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]] : $()

// CHECK-LABEL sil hidden [transparent] [thunk] @_T017materializeForSet23GenericSubscriptWitnessVAA0dE8ProtocolAaaDP9subscriptqd__qd__clufmTW : $@convention(witness_method) <τ_0_0> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @in τ_0_0, @inout GenericSubscriptWitness) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK:      bb0(%0 : $Builtin.RawPointer, %1 : $*Builtin.UnsafeValueBuffer, %2 : $*T, %3 : $*GenericSubscriptWitness):
// CHECK-NEXT:   [[BUFFER:%.*]] = alloc_value_buffer $T in %1 : $*Builtin.UnsafeValueBuffer
// CHECK-NEXT:   copy_addr %2 to [initialization] [[BUFFER]] : $*T
// CHECK-NEXT:   [[VALUE:%.*]] = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*T
// CHECK-NEXT:   [[SELF:%.*]] = load [trivial] %3 : $*GenericSubscriptWitness
// CHECK:        [[GETTER:%.*]] = function_ref @_T017materializeForSet23GenericSubscriptWitnessV9subscriptxxclufg : $@convention(method) <τ_0_0> (@in τ_0_0, GenericSubscriptWitness) -> @out τ_0_0
// CHECK-NEXT:   apply [[GETTER]]<T>([[VALUE]], %2, [[SELF]]) : $@convention(method) <τ_0_0> (@in τ_0_0, GenericSubscriptWitness) -> @out τ_0_0
// CHECK-NEXT:   [[VALUE_PTR:%.*]] = address_to_pointer [[VALUE]] : $*T to $Builtin.RawPointer
// CHECK:        [[CALLBACK:%.*]] = function_ref @_T017materializeForSet23GenericSubscriptWitnessV9subscriptxxclufmytfU_
// CHECK-NEXT:   [[CALLBACK_PTR:%.*]] = thin_function_to_pointer [[CALLBACK]] : $@convention(method) <τ_0_0> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout GenericSubscriptWitness, @thick GenericSubscriptWitness.Type) -> () to $Builtin.RawPointer
// CHECK-NEXT:   [[CALLBACK_OPTIONAL:%.*]] = enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, [[CALLBACK_PTR]] : $Builtin.RawPointer
// CHECK-NEXT:   [[RESULT:%.*]] = tuple ([[VALUE_PTR]] : $Builtin.RawPointer, [[CALLBACK_OPTIONAL]] : $Optional<Builtin.RawPointer>)
// CHECK-NEXT:   return [[RESULT]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>)

extension GenericSubscriptProtocol {
  subscript<T>(t: T) -> T { get { } set { } }
}

struct GenericSubscriptDefaultWitness : GenericSubscriptProtocol { }

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

// Test for materializeForSet vs overridden computed property of classes.
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

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet11TuxedoPandaVAA0E0AaaDP1xxxcfmytfU_TW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout TuxedoPanda, @thick TuxedoPanda.Type) -> ()

  // FIXME: Useless re-abstractions

  // CHECK: function_ref @_T017materializeForSet11TuxedoPandaVACIxir_AcCIxyd_TR : $@convention(thin) (TuxedoPanda, @owned @callee_owned (@in TuxedoPanda) -> @out TuxedoPanda) -> TuxedoPanda

  // CHECK: function_ref @_T017materializeForSet5PandaPAAE1xxxcfs : $@convention(method) <τ_0_0 where τ_0_0 : Panda> (@owned @callee_owned (@in τ_0_0) -> @out τ_0_0, @inout τ_0_0) -> ()

  // CHECK: function_ref @_T017materializeForSet11TuxedoPandaVACIxyd_AcCIxir_TR : $@convention(thin) (@in TuxedoPanda, @owned @callee_owned (TuxedoPanda) -> TuxedoPanda) -> @out TuxedoPanda

// CHECK: }

// CHECK-LABEL: sil hidden [transparent] [thunk] @_T017materializeForSet11TuxedoPandaVAA0E0AaaDP1xxxcfmTW

// Call the getter:

  // CHECK: function_ref @_T017materializeForSet5PandaPAAE1xxxcfg : $@convention(method) <τ_0_0 where τ_0_0 : Panda> (@in_guaranteed τ_0_0) -> @owned @callee_owned (@in τ_0_0) -> @out τ_0_0

// Result of calling the getter is re-abstracted to the maximally substituted type
// by SILGenFunction::emitApply():

  // CHECK: function_ref @_T017materializeForSet11TuxedoPandaVACIxir_AcCIxyd_TR : $@convention(thin) (TuxedoPanda, @owned @callee_owned (@in TuxedoPanda) -> @out TuxedoPanda) -> TuxedoPanda

// ... then we re-abstract to the requirement signature:
// FIXME: Peephole this away with the previous one since there's actually no
// abstraction change in this case.

  // CHECK: function_ref @_T017materializeForSet11TuxedoPandaVACIxyd_AcCIxir_TR : $@convention(thin) (@in TuxedoPanda, @owned @callee_owned (TuxedoPanda) -> TuxedoPanda) -> @out TuxedoPanda

// The callback:

  // CHECK: function_ref @_T017materializeForSet11TuxedoPandaVAA0E0AaaDP1xxxcfmytfU_TW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout TuxedoPanda, @thick TuxedoPanda.Type) -> ()

// CHECK: }


// Test for materializeForSet vs lazy properties of structs.

struct LazyStructProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_T017materializeForSet31inoutAccessOfLazyStructPropertyyAA0ghI0Vz1l_tF
// CHECK:   function_ref @_T017materializeForSet18LazyStructPropertyV3catSifg
// CHECK:   function_ref @_T017materializeForSet18LazyStructPropertyV3catSifs
func inoutAccessOfLazyStructProperty(l: inout LazyStructProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of classes.

// CHECK-LABEL: sil hidden [transparent] @_T017materializeForSet17LazyClassPropertyC3catSifm

class LazyClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_T017materializeForSet30inoutAccessOfLazyClassPropertyyAA0ghI0Cz1l_tF
// CHECK:    class_method {{.*}} : $LazyClassProperty, #LazyClassProperty.cat!materializeForSet.1
func inoutAccessOfLazyClassProperty(l: inout LazyClassProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of final classes.

final class LazyFinalClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden @_T017materializeForSet35inoutAccessOfLazyFinalClassPropertyyAA0ghiJ0Cz1l_tF
// CHECK:    function_ref @_T017materializeForSet22LazyFinalClassPropertyC3catSifg
// CHECK:    function_ref @_T017materializeForSet22LazyFinalClassPropertyC3catSifs
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

// CHECK-LABEL: _T017materializeForSet22testMaterializedSetteryyF
func testMaterializedSetter() {
  // CHECK: function_ref @_T017materializeForSet10FooClosureVACycfC
  var f = FooClosure()
  // CHECK: function_ref @_T017materializeForSet10FooClosureV8computedSiSiSiccSgfg
  // CHECK: function_ref @_T017materializeForSet10FooClosureV8computedSiSiSiccSgfs
  f.computed = f.computed
}

// CHECK-LABEL: sil_vtable DerivedForOverride {
// CHECK:   #BaseForOverride.valueComputed!getter.1: (BaseForOverride) -> () -> Int : _T017materializeForSet07DerivedB8OverrideC13valueComputedSifg
// CHECK:   #BaseForOverride.valueComputed!setter.1: (BaseForOverride) -> (Int) -> () : _T017materializeForSet07DerivedB8OverrideC13valueComputedSifs
// CHECK:   #BaseForOverride.valueComputed!materializeForSet.1: (BaseForOverride) -> (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer) -> (Builtin.RawPointer, Builtin.RawPointer?) : _T017materializeForSet07DerivedB8OverrideC13valueComputedSifm
// CHECK:   #BaseForOverride.valueStored!getter.1: (BaseForOverride) -> () -> Int : _T017materializeForSet07DerivedB8OverrideC11valueStoredSifg
// CHECK:   #BaseForOverride.valueStored!setter.1: (BaseForOverride) -> (Int) -> () : _T017materializeForSet07DerivedB8OverrideC11valueStoredSifs
// CHECK:   #BaseForOverride.valueStored!materializeForSet.1: (BaseForOverride) -> (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer) -> (Builtin.RawPointer, Builtin.RawPointer?) : _T017materializeForSet07DerivedB8OverrideC11valueStoredSifm
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden Bill: Totalled module materializeForSet {
// CHECK:   method #Totalled.total!getter.1: {{.*}} : @_T017materializeForSet4BillVAA8TotalledAaaDP5totalSifgTW
// CHECK:   method #Totalled.total!setter.1: {{.*}} : @_T017materializeForSet4BillVAA8TotalledAaaDP5totalSifsTW
// CHECK:   method #Totalled.total!materializeForSet.1: {{.*}} : @_T017materializeForSet4BillVAA8TotalledAaaDP5totalSifmTW
// CHECK: }
