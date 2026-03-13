
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name modify %s | %FileCheck %s

class Base {
  var stored: Int = 0

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify4BaseC6storedSivM : $@yield_once @convention(method) (@guaranteed Base) -> @yields @inout Int {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $Base):
// CHECK:   [[T0:%.*]] = ref_element_addr [[SELF]] : $Base, #Base.stored
// CHECK:   [[T1:%.*]] = begin_access [modify] [dynamic] [[T0]]
// CHECK:   yield [[T1]] : $*Int
// CHECK:   end_access [[T1]]
// CHECK:   return {{%.*}} : $()
// CHECK: }

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify4BaseC8computedSivM : $@yield_once @convention(method) (@guaranteed Base) -> @yields @inout Int {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $Base):
// CHECK:   [[ADDR:%.*]] = alloc_stack $Int
// CHECK:   [[T0:%.*]] = function_ref @$s6modify4BaseC8computedSivg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[ADDR]] : $*Int
// CHECK:   yield [[ADDR]] : $*Int
// CHECK:   [[T2:%.*]] = load [trivial] [[ADDR]] : $*Int
// CHECK:   [[SETTER:%.*]] = function_ref @$s6modify4BaseC8computedSivs
// CHECK:   apply [[SETTER]]([[T2]], [[SELF]])
// CHECK:   return {{%.*}} : $()
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

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s6modify7DerivedCAA12AbstractableA2aDP14storedFunction6ResultQzycvMTW
// CHECK: bb0(%0 : $*Derived):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %0 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: [[FN:%.*]] = class_method [[SELF]] : $Base, #Base.storedFunction!modify
// CHECK-NEXT: ([[SUPER_ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: [[SUB_ADDR:%.*]] = alloc_stack $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>
// CHECK-NEXT: [[SUPER_FN:%.*]] = load [take] [[SUPER_ADDR]]
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegd_SiIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[SUPER_FN]])
// CHECK-NEXT: [[T2:%.*]] = convert_function [[T1]]
// CHECK-NEXT: store [[T2]] to [init] [[SUB_ADDR]]
// CHECK-NEXT: yield [[SUB_ADDR]] : ${{.*}}, resume bb1, unwind bb2
// CHECK:    bb1:
// CHECK-NEXT: [[SUB_FN:%.*]] = load [take] [[SUB_ADDR]]
// CHECK-NEXT: [[CVT_FN:%.*]] = convert_function [[SUB_FN]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegr_SiIegd_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @out Int) -> Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[CVT_FN]])
// CHECK-NEXT: store [[T1]] to [init] [[SUPER_ADDR]]
// CHECK-NEXT: dealloc_stack [[SUB_ADDR]]
// CHECK-NEXT: end_apply [[TOKEN]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: end_borrow [[T0]]
// CHECK-NEXT: return

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s6modify7DerivedCAA12AbstractableA2aDP19finalStoredFunction6ResultQzycvMTW
// CHECK: bb0(%0 : $*Derived):
// CHECK-NEXT: [[T0:%.*]] = load_borrow %0 : $*Derived
// CHECK-NEXT: [[SELF:%.*]] = upcast [[T0]] : $Derived to $Base
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$s6modify4BaseC19finalStoredFunctionSiycvM
// CHECK-NEXT: ([[SUPER_ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: [[SUB_ADDR:%.*]] = alloc_stack $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>
// CHECK-NEXT: [[SUPER_FN:%.*]] = load [take] [[SUPER_ADDR]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegd_SiIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[SUPER_FN]])
// CHECK-NEXT: [[T2:%.*]] = convert_function [[T1]]
// CHECK-NEXT: store [[T2]] to [init] [[SUB_ADDR]]
// CHECK-NEXT: yield [[SUB_ADDR]] : $*{{.*}}, resume bb1, unwind bb2
// CHECK:    bb1:
// CHECK-NEXT: [[SUB_FN:%.*]] = load [take] [[SUB_ADDR]]
// CHECK-NEXT: [[CVT_FN:%.*]] = convert_function [[SUB_FN]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegr_SiIegd_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @out Int) -> Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[CVT_FN]])
// CHECK-NEXT: store [[T1]] to [init] [[SUPER_ADDR]]
// CHECK-NEXT: dealloc_stack [[SUB_ADDR]]
// CHECK-NEXT: end_apply [[TOKEN]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: end_borrow [[T0]]
// CHECK-NEXT: return

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s6modify7DerivedCAA12AbstractableA2aDP14staticFunction6ResultQzycvMZTW
// CHECK: bb0(%0 : $@thick Derived.Type):
// CHECK-NEXT: [[SELF:%.*]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$s6modify4BaseC14staticFunctionSiycvMZ
// CHECK-NEXT: ([[SUPER_ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: [[SUB_ADDR:%.*]] = alloc_stack $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Int>
// CHECK-NEXT: [[SUPER_FN:%.*]] = load [take] [[SUPER_ADDR]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegd_SiIegr_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> Int) -> @out Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[SUPER_FN]])
// CHECK-NEXT: [[T2:%.*]] = convert_function [[T1]]
// CHECK-NEXT: store [[T2]] to [init] [[SUB_ADDR]]
// CHECK-NEXT: yield [[SUB_ADDR]] : $*{{.*}}, resume bb1, unwind bb2
// CHECK:    bb1:
// CHECK-NEXT: [[SUB_FN:%.*]] = load [take] [[SUB_ADDR]]
// CHECK-NEXT: [[CVT_FN:%.*]] = convert_function [[SUB_FN]]
// CHECK-NEXT: function_ref
// CHECK-NEXT: [[REABSTRACTOR:%.*]] = function_ref @$sSiIegr_SiIegd_TR : $@convention(thin) (@guaranteed @callee_guaranteed () -> @out Int) -> Int
// CHECK-NEXT: [[T1:%.*]] = partial_apply [callee_guaranteed] [[REABSTRACTOR]]([[CVT_FN]])
// CHECK-NEXT: store [[T1]] to [init] [[SUPER_ADDR]]
// CHECK-NEXT: dealloc_stack [[SUB_ADDR]]
// CHECK-NEXT: end_apply [[TOKEN]]
// CHECK-NEXT: tuple ()
// CHECK-NEXT: return

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

// CHECK-LABEL: sil hidden [ossa] @$s6modify9HasDidSetC6storedSivM : $@yield_once @convention(method) (@guaranteed HasDidSet) -> @yields @inout Int {
// CHECK:      bb0([[SELF:%.*]] : @guaranteed $HasDidSet):
// CHECK:        [[COPY_VAL:%.*]] = copy_value [[SELF]] : $HasDidSet
// CHECK-NEXT:   [[UPCAST:%.*]] = upcast [[COPY_VAL]] : $HasDidSet to $Base
// CHECK-NEXT:   [[BEGIN_BORROW:%.*]] = begin_borrow [[UPCAST]]
// CHECK-NEXT:   // function_ref Base.stored.modify
// CHECK-NEXT:   [[FUNC_REF:%.*]] = function_ref @$s6modify4BaseC6storedSivM
// CHECK-NEXT:   ([[VAL:%.*]], [[IGNORE:%.*]]) = begin_apply [[FUNC_REF]]([[BEGIN_BORROW]])
// CHECK-NEXT:   yield [[VAL]] : $*Int, resume bb1, unwind bb2
// CHECK:        [[DIDSET:%.*]] = function_ref @$s6modify9HasDidSetC6storedSivW
// CHECK-NEXT:   apply [[DIDSET]]([[SELF]])
// CHECK-NEXT:   [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:   return [[TUPLE]]
// CHECK: }

  override var computed: Int {
    get { return 0 }
    set(value) {}
  }

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify9HasDidSetC8computedSivM : $@yield_once @convention(method) (@guaranteed HasDidSet) -> @yields @inout Int {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $HasDidSet):
// CHECK:   [[ADDR:%.*]] = alloc_stack $Int
// CHECK:   [[T0:%.*]] = function_ref @$s6modify9HasDidSetC8computedSivg
// CHECK:   [[T1:%.*]] = apply [[T0]]([[SELF]])
// CHECK:   store [[T1]] to [trivial] [[ADDR]] : $*Int
// CHECK:   yield [[ADDR]]
// CHECK:   [[T2:%.*]] = load [trivial] [[ADDR]]
// CHECK:   [[T3:%.*]] = function_ref @$s6modify9HasDidSetC8computedSivs
// CHECK:   apply [[T3]]([[T2]], [[SELF]])
// CHECK:   dealloc_stack [[ADDR]]
// CHECK:   return {{%.*}} : $()
// CHECK: }
}

class HasStoredDidSet {
  var stored: Int = 0 {
    didSet {}
  }

// sil hidden [ossa] @$s6modify15HasStoredDidSetC6storedSivM : $@yield_once @convention(method) (@guaranteed HasStoredDidSet) -> @yields @inout Int {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $HasStoredDidSet):
// CHECK:   [[REF_ADDR:%.*]] = ref_element_addr [[SELF]] : $HasStoredDidSet, #HasStoredDidSet.stored
// CHECK:   [[BEGIN_ACCESS:%.*]] = begin_access [modify] [dynamic] [[REF_ADDR]] : $*Int
// CHECK:   yield [[BEGIN_ACCESS]]
// CHECK:   [[DIDSET:%.*]] = function_ref @$s6modify15HasStoredDidSetC6storedSivW
// CHECK:   apply [[DIDSET]]([[SELF]])
// CHECK:   [[TUPLE:%.*]] = tuple ()
// CHECK:   return [[TUPLE]]
// CHECK: }
}

class HasWeak {
  weak var weakvar: HasWeak?
}
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify7HasWeakC7weakvarACSgvM : $@yield_once @convention(method) (@guaranteed HasWeak) -> @yields @inout Optional<HasWeak> {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $HasWeak):
// CHECK:   [[PROP:%.*]] = ref_element_addr [[SELF]] : $HasWeak, #HasWeak.weakvar
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[PROP]] : $*@sil_weak Optional<HasWeak>
// CHECK:   [[ADDR:%.*]] = alloc_stack $Optional<HasWeak>
// CHECK:   [[T0:%.*]] = load_weak [[ACCESS]]
// CHECK:   store [[T0]] to [init] [[ADDR]]
// CHECK:   yield [[ADDR]]
// CHECK:   [[T0:%.*]] = load [take] [[ADDR]]
// CHECK:   store_weak [[T0]] to [[ACCESS]]
// CHECK:   destroy_value [[T0]]
// CHECK:   end_access [[ACCESS]]
// CHECK:   dealloc_stack [[ADDR]]
// CHECK:   return {{%.*}} : $()
// CHECK: }

// rdar://22109071
// Test that we don't use modify from a protocol extension.
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
// CHECK-LABEL: sil hidden [ossa] @$s6modify13improveWizardyyAA0C0VzF
// CHECK:       [[WRITE:%.*]] = begin_access [modify] [unknown] %0 : $*Wizard
// CHECK-NEXT:  [[TEMP:%.*]] = alloc_stack $Int
//   Call the getter and materialize the result in the temporary.
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[WRITE:.*]] : $*Wizard
// CHECK:       [[WTEMP:%.*]] = alloc_stack $Wizard
// CHECK-NEXT:  store [[T0]] to [trivial] [[WTEMP]]
// CHECK:       [[GETTER:%.*]] = function_ref @$s6modify5MagicPAAE5hocusSivg
// CHECK-NEXT:  [[T0:%.*]] = apply [[GETTER]]<Wizard>([[WTEMP]])
// CHECK-NEXT:  dealloc_stack [[WTEMP]]
// CHECK-NEXT:  store [[T0]] to [trivial] [[TEMP]]
//   Call improve.
// CHECK:       [[IMPROVE:%.*]] = function_ref @$s6modify7improveyySizF :
// CHECK-NEXT:  apply [[IMPROVE]]([[TEMP]])
// CHECK-NEXT:  [[T0:%.*]] = load [trivial] [[TEMP]]
// CHECK:       [[SETTER:%.*]] = function_ref @$s6modify5MagicPAAE5hocusSivs
// CHECK-NEXT:  apply [[SETTER]]<Wizard>([[T0]], [[WRITE]])
// CHECK-NEXT:  end_access [[WRITE]] : $*Wizard
// CHECK-NEXT:  dealloc_stack [[TEMP]]

protocol Totalled {
  var total: Int { get set }
}

struct Bill : Totalled {
  var total: Int
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify4BillV5totalSivM : $@yield_once @convention(method) (@inout Bill) -> @yields @inout Int {
// CHECK: bb0([[SELF:%.*]] : $*Bill):
// CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[SELF]]
// CHECK:   [[T0:%.*]] = struct_element_addr [[ACCESS]] : $*Bill, #Bill.total
// CHECK:   yield [[T0]]
// CHECK:   end_access [[ACCESS]]
// CHECK: }

// CHECK-LABEL:  sil private [transparent] [thunk] [ossa] @$s6modify4BillVAA8TotalledA2aDP5totalSivMTW :
// CHECK:        bb0([[SELF:%.*]] : $*Bill):
// CHECK:          [[T0:%.*]] = function_ref @$s6modify4BillV5totalSivM
// CHECK-NEXT:     ([[T1:%.*]], [[TOKEN:%.*]]) = begin_apply [[T0]]([[SELF]])
// CHECK-NEXT:     yield [[T1]] : $*Int, resume bb1, unwind bb2
// CHECK:        bb1:
// CHECK-NEXT:     end_apply [[TOKEN]]
// CHECK-NEXT:     [[T1:%.*]] = tuple ()
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

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify23GenericSubscriptWitnessVyxxcluiM
// CHECK:         function_ref @$s6modify23GenericSubscriptWitnessVyxxcluig
// CHECK:         function_ref @$s6modify23GenericSubscriptWitnessVyxxcluis

extension GenericSubscriptProtocol {
  subscript<T>(t: T) -> T { get { } set { } }
}

struct GenericSubscriptDefaultWitness : GenericSubscriptProtocol { }

// Make sure we correctly infer the 'T : Magic' requirement on all the accessors
// of the subscript.

struct GenericTypeWithRequirement<T : Magic> {}

protocol InferredRequirementOnSubscriptProtocol {
  subscript<T>(i: Int) -> GenericTypeWithRequirement<T> { get set }
}

struct InferredRequirementOnSubscript : InferredRequirementOnSubscriptProtocol {
  subscript<T>(i: Int) -> GenericTypeWithRequirement<T> {
    get { }
    set { }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s6modify30InferredRequirementOnSubscriptVyAA015GenericTypeWithC0VyxGSicAA5MagicRzluig : $@convention(method) <T where T : Magic> (Int, InferredRequirementOnSubscript) -> GenericTypeWithRequirement<T>

// CHECK-LABEL: sil hidden [ossa] @$s6modify30InferredRequirementOnSubscriptVyAA015GenericTypeWithC0VyxGSicAA5MagicRzluis : $@convention(method) <T where T : Magic> (GenericTypeWithRequirement<T>, Int, @inout InferredRequirementOnSubscript) -> ()

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify30InferredRequirementOnSubscriptVyAA015GenericTypeWithC0VyxGSicAA5MagicRzluiM : $@yield_once @convention(method) <T where T : Magic> (Int, @inout InferredRequirementOnSubscript) -> @yields @inout GenericTypeWithRequirement<T>

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


// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s6modify11TuxedoPandaVAA0C0A2aDP1xyxxcvMTW

// CHECK: [[T0:%.*]] = function_ref @$s6modify5PandaPAAE1xyxxcvM
// CHECK: ([[T1:%.*]], [[TOKEN:%.*]]) = begin_apply [[T0]]<TuxedoPanda>(%0)
// CHECK: yield [[T1]]
// CHECK: end_apply [[TOKEN]]
// CHECK: }


// Test for materializeForSet vs lazy properties of structs.

struct LazyStructProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden [ossa] @$s6modify31inoutAccessOfLazyStructProperty1lyAA0efG0Vz_tF
// CHECK:   function_ref @$s6modify18LazyStructPropertyV3catSivg
// CHECK:   function_ref @$s6modify18LazyStructPropertyV3catSivs
func inoutAccessOfLazyStructProperty(l: inout LazyStructProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of classes.

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify17LazyClassPropertyC3catSivM

class LazyClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden [ossa] @$s6modify30inoutAccessOfLazyClassProperty1lyAA0efG0Cz_tF
// CHECK:    class_method {{.*}} : $LazyClassProperty, #LazyClassProperty.cat!modify
func inoutAccessOfLazyClassProperty(l: inout LazyClassProperty) {
  increment(&l.cat)
}

// Test for materializeForSet vs lazy properties of final classes.

final class LazyFinalClassProperty {
  lazy var cat: Int = 5
}

// CHECK-LABEL: sil hidden [ossa] @$s6modify35inoutAccessOfLazyFinalClassProperty1lyAA0efgH0Cz_tF
// CHECK:    function_ref @$s6modify22LazyFinalClassPropertyC3catSivg
// CHECK:    function_ref @$s6modify22LazyFinalClassPropertyC3catSivs
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

// CHECK-LABEL: $s6modify22testMaterializedSetteryyF
func testMaterializedSetter() {
  // CHECK: function_ref @$s6modify10FooClosureVACycfC
  var f = FooClosure()
  // CHECK: function_ref @$s6modify10FooClosureV8computedS3iXEcSgvg
  // CHECK: function_ref @$s6modify10FooClosureV8computedS3iXEcSgvs
  f.computed = f.computed
}

// Odd corner case -- mutating getter, non-mutating setter
protocol BackwardMutationProtocol {
  var value: Int {
    mutating get
    nonmutating set
  }
}

struct BackwardMutation : BackwardMutationProtocol {
  var value: Int {
    mutating get { return 0 }
    nonmutating set { }
  }
}

func doBackwardMutation(m: inout BackwardMutationProtocol) {
  m.value += 1
}

// materializeForSet for a constrained-extension protocol witness requires
// open coding.

protocol ConditionalSubscript {
  subscript(_: Int) -> Self { get set }
}

struct HasConditionalSubscript<T> {}

extension HasConditionalSubscript: ConditionalSubscript where T: ConditionalSubscript {
  subscript(_: Int) -> HasConditionalSubscript<T> {
    get { return self }
    set { }
  }
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s6modify23HasConditionalSubscriptVA2A0cD0RzlEyACyxGSiciM
// CHECK:         function_ref @$s6modify23HasConditionalSubscriptVA2A0cD0RzlEyACyxGSicig

// CHECK-LABEL: sil_vtable DerivedForOverride {
// CHECK:   #BaseForOverride.valueStored!getter: (BaseForOverride) -> () -> Int : @$s6modify18DerivedForOverrideC11valueStoredSivg
// CHECK:   #BaseForOverride.valueStored!setter: (BaseForOverride) -> (Int) -> () : @$s6modify18DerivedForOverrideC11valueStoredSivs
// CHECK:   #BaseForOverride.valueStored!modify: (BaseForOverride) -> () -> () : @$s6modify18DerivedForOverrideC11valueStoredSivM
// CHECK:   #BaseForOverride.valueComputed!getter: (BaseForOverride) -> () -> Int : @$s6modify18DerivedForOverrideC13valueComputedSivg
// CHECK:   #BaseForOverride.valueComputed!setter: (BaseForOverride) -> (Int) -> () : @$s6modify18DerivedForOverrideC13valueComputedSivs
// CHECK:   #BaseForOverride.valueComputed!modify: (BaseForOverride) -> () -> () : @$s6modify18DerivedForOverrideC13valueComputedSivM
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden Bill: Totalled module modify {
// CHECK:   method #Totalled.total!getter: {{.*}} : @$s6modify4BillVAA8TotalledA2aDP5totalSivgTW
// CHECK:   method #Totalled.total!setter: {{.*}} : @$s6modify4BillVAA8TotalledA2aDP5totalSivsTW
// CHECK:   method #Totalled.total!modify: {{.*}} : @$s6modify4BillVAA8TotalledA2aDP5totalSivMTW
// CHECK: }
