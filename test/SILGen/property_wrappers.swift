// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/property_wrapper_defs.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -primary-file %s -I %t | %FileCheck %s
import property_wrapper_defs

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T

  init(value: T) {
    wrappedValue = value
  }
}

@propertyWrapper
struct WrapperWithInitialValue<T> {
  var wrappedValue: T
}

protocol DefaultInit {
  init()
}

extension Int: DefaultInit { }

struct HasMemberwiseInit<T: DefaultInit> {
  @Wrapper(value: false)
  var x: Bool

  @WrapperWithInitialValue
  var y: T = T()

  @WrapperWithInitialValue(wrappedValue: 17)
  var z: Int

  @WrapperWithInitialValue
  private var p: Bool = true
}

func forceHasMemberwiseInit() {
  _ = HasMemberwiseInit(x: Wrapper(value: true), y: 17, z: WrapperWithInitialValue(wrappedValue: 42))
  _ = HasMemberwiseInit<Int>(x: Wrapper(value: true))
  _ = HasMemberwiseInit(y: 17)
  _ = HasMemberwiseInit<Int>(z: WrapperWithInitialValue(wrappedValue: 42))
  _ = HasMemberwiseInit<Int>()
}

  // CHECK: sil_global private @$s17property_wrappers9UseStaticV13_staticWibble33_{{.*}}AA4LazyOySaySiGGvpZ : $Lazy<Array<Int>>

// HasMemberwiseInit.x.setter
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1xSbvs : $@convention(method) <T where T : DefaultInit> (Bool, @inout HasMemberwiseInit<T>) -> () {
// CHECK: bb0(%0 : $Bool, %1 : $*HasMemberwiseInit<T>):
// CHECK: [[MODIFY_SELF:%.*]] = begin_access [modify] [unknown] %1 : $*HasMemberwiseInit<T>
// CHECK: [[X_BACKING:%.*]] = struct_element_addr [[MODIFY_SELF]] : $*HasMemberwiseInit<T>, #HasMemberwiseInit._x
// CHECK: [[X_BACKING_VALUE:%.*]] = struct_element_addr [[X_BACKING]] : $*Wrapper<Bool>, #Wrapper.wrappedValue
// CHECK: assign %0 to [[X_BACKING_VALUE]] : $*Bool
// CHECK: end_access [[MODIFY_SELF]] : $*HasMemberwiseInit<T>

// variable initialization expression of HasMemberwiseInit._x
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers17HasMemberwiseInitV2_x33_{{.*}}AA7WrapperVySbGvpfi : $@convention(thin) <T where T : DefaultInit> () -> Wrapper<Bool> {
// CHECK: integer_literal $Builtin.Int1, 0
// CHECK-NOT: return
// CHECK: function_ref @$sSb22_builtinBooleanLiteralSbBi1__tcfC : $@convention(method) (Builtin.Int1, @thin Bool.Type) -> Bool
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers7WrapperV5valueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0> // user: %9
// CHECK: return {{%.*}} : $Wrapper<Bool>

// variable initialization expression of HasMemberwiseInit.$y
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers17HasMemberwiseInitV2_y33_{{.*}}23WrapperWithInitialValueVyxGvpfi : $@convention(thin) <T where T : DefaultInit> () -> @out 
// CHECK: bb0(%0 : $*T):
// CHECK-NOT: return
// CHECK: witness_method $T, #DefaultInit.init!allocator : <Self where Self : DefaultInit> (Self.Type) -> () -> Self : $@convention(witness_method: DefaultInit) <τ_0_0 where τ_0_0 : DefaultInit> (@thick τ_0_0.Type) -> @out τ_0_0

// variable initialization expression of HasMemberwiseInit._z
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers17HasMemberwiseInitV2_z33_{{.*}}23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {
// CHECK: bb0:
// CHECK-NOT: return
// CHECK: integer_literal $Builtin.IntLiteral, 17
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers23WrapperWithInitialValueV07wrappedF0ACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin WrapperWithInitialValue<τ_0_0>.Type) -> @out WrapperWithInitialValue<τ_0_0>

// variable initialization expression of HasMemberwiseInit._p
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers17HasMemberwiseInitV2_p33_{{.*}}23WrapperWithInitialValueVySbGvpfi : $@convention(thin) <T where T : DefaultInit> () -> Bool {
// CHECK: function_ref @$sSb22_builtinBooleanLiteralSbBi1__tcfC : $@convention(method) (Builtin.Int1, @thin Bool.Type) -> Bool
// CHECK: return {{%.*}} : $Bool

// HasMemberwiseInit.init()
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitVACyxGycfC : $@convention(method) <T where T : DefaultInit> (@thin HasMemberwiseInit<T>.Type) -> @out HasMemberwiseInit<T> {

// Initialization of x
// CHECK-NOT: return %
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_x33_{{.*}}7WrapperVySbGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> Wrapper<Bool>

// Initialization of y
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_y33_{{.*}}23WrapperWithInitialValueVyxGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> @out τ_0_0
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV1yxvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@in τ_0_0) -> @out WrapperWithInitialValue<τ_0_0>

// Initialization of z
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_z33_{{.*}}23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> WrapperWithInitialValue<Int>

// Initialization of p
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_p33_{{.*}}23WrapperWithInitialValueVySbGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> Bool
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV1p33_{{.*}} : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (Bool) -> WrapperWithInitialValue<Bool>

// CHECK: return


// default argument 0 of HasMemberwiseInit.init(x:y:z:)
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA_ : $@convention(thin) <T where T : DefaultInit> () -> Wrapper<Bool> 

// default argument 1 of HasMemberwiseInit.init(x:y:z:)
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA0_ : $@convention(thin) <T where T : DefaultInit> () -> @out T {

// default argument 2 of HasMemberwiseInit.init(x:y:z:)
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA1_ : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {


// Non-generic struct with private property wrapper
struct HasMemberwiseInitWithPrivateWrapper {
  @WrapperWithInitialValue
  var z: Int = 17

  @WrapperWithInitialValue
  private var p: Bool = true

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers35HasMemberwiseInitWithPrivateWrapperV1zACSi_tcfC : $@convention(method) (Int, @thin HasMemberwiseInitWithPrivateWrapper.Type) -> HasMemberwiseInitWithPrivateWrapper {
  // CHECK: function_ref @$s17property_wrappers35HasMemberwiseInitWithPrivateWrapperV1zSivpfP : $@convention(thin) (Int) -> WrapperWithInitialValue<Int>
  // CHECK: function_ref @$s17property_wrappers35HasMemberwiseInitWithPrivateWrapperV1p33_{{.*}} : $@convention(thin) (Bool) -> WrapperWithInitialValue<Bool>
  // CHECK: return {{%.*}} : $HasMemberwiseInitWithPrivateWrapper
}

func forceHasMemberwiseInitWithPrivateWrapper() {
  _ = HasMemberwiseInitWithPrivateWrapper(z: 42)
}


// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers9HasNestedV2_y33_{{.*}}14PrivateWrapperAELLVyx_SayxGGvpfi : $@convention(thin) <T> () -> @owned Array<T> {
// CHECK: bb0:
// CHECK: function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
struct HasNested<T> {
  @propertyWrapper
  private struct PrivateWrapper<U> {
    var wrappedValue: U

  }

  @PrivateWrapper
  private var y: [T] = []

  static func blah(y: [T]) -> HasNested<T> {
    return HasNested<T>()
  }
}

// FIXME: For now, we are only checking that we don't crash.
struct HasDefaultInit {
  @Wrapper(value: true)
  var x

  @WrapperWithInitialValue
  var y = 25

  static func defaultInit() -> HasDefaultInit {
    return HasDefaultInit()
  }

  static func memberwiseInit(x: Bool, y: Int) -> HasDefaultInit {
    return HasDefaultInit(x: Wrapper(value: x), y: y)
  }
}

struct WrapperWithAccessors {
  @Wrapper
  var x: Int

  // Synthesized setter
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers20WrapperWithAccessorsV1xSivs : $@convention(method) (Int, @inout WrapperWithAccessors) -> ()
  // CHECK-NOT: return
  // CHECK: struct_element_addr {{%.*}} : $*WrapperWithAccessors, #WrapperWithAccessors._x

  mutating func test() {
    x = 17
  }
}

func consumeOldValue(_: Int) { }
func consumeNewValue(_: Int) { }

struct WrapperWithDidSetWillSet {
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers021WrapperWithDidSetWillF0V1xSivs
  // CHECK: function_ref @$s17property_wrappers021WrapperWithDidSetWillF0V1xSivw
  // CHECK: struct_element_addr {{%.*}} : $*WrapperWithDidSetWillSet, #WrapperWithDidSetWillSet._x
  // CHECK-NEXT: struct_element_addr {{%.*}} : $*Wrapper<Int>, #Wrapper.wrappedValue
  // CHECK-NEXT: assign %0 to {{%.*}} : $*Int
  // CHECK: function_ref @$s17property_wrappers021WrapperWithDidSetWillF0V1xSivW
  @Wrapper
  var x: Int {
    didSet {
      consumeNewValue(oldValue)
    }

    willSet {
      consumeOldValue(newValue)
    }
  }

  mutating func test(x: Int) {
    self.x = x
  }
}

@propertyWrapper
struct WrapperWithStorageValue<T> {
  var wrappedValue: T

  var projectedValue: Wrapper<T> {
    return Wrapper(value: wrappedValue)
  }
}

struct UseWrapperWithStorageValue {
  // UseWrapperWithStorageValue._x.getter
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers26UseWrapperWithStorageValueV2$xAA0D0VySiGvg : $@convention(method) (UseWrapperWithStorageValue) -> Wrapper<Int>
  // CHECK-NOT: return
  // CHECK: function_ref @$s17property_wrappers23WrapperWithStorageValueV09projectedF0AA0C0VyxGvg
  @WrapperWithStorageValue(wrappedValue: 17) var x: Int
}

@propertyWrapper
enum Lazy<Value> {
  case uninitialized(() -> Value)
  case initialized(Value)

  init(wrappedValue initialValue: @autoclosure @escaping () -> Value) {
    self = .uninitialized(initialValue)
  }

  var wrappedValue: Value {
    mutating get {
      switch self {
      case .uninitialized(let initializer):
        let value = initializer()
        self = .initialized(value)
        return value
      case .initialized(let value):
        return value
      }
    }
    set {
      self = .initialized(newValue)
    }
  }
}

struct UseLazy<T: DefaultInit> {
  @Lazy var foo = 17
  @Lazy var bar = T()
  @Lazy var wibble = [1, 2, 3]

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers7UseLazyV3foo3bar6wibbleACyxGSiyXA_xyXASaySiGyXAtcfC : $@convention(method) <T where T : DefaultInit> (@owned @callee_guaranteed () -> Int, @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T>, @owned @callee_guaranteed () -> @owned Array<Int>, @thin UseLazy<T>.Type) -> @out UseLazy<T> {
  // CHECK: function_ref @$s17property_wrappers7UseLazyV3fooSivpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@owned @callee_guaranteed () -> Int) -> @owned Lazy<Int>
  // CHECK: function_ref @$s17property_wrappers7UseLazyV3barxvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>) -> @out Lazy<τ_0_0>
  // CHECK: function_ref @$s17property_wrappers7UseLazyV6wibbleSaySiGvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@owned @callee_guaranteed () -> @owned Array<Int>) -> @owned Lazy<Array<Int>>
}

struct X { }

func triggerUseLazy() {
  _ = UseLazy<Int>()
  _ = UseLazy<Int>(foo: 17)
  _ = UseLazy(bar: 17)
  _ = UseLazy<Int>(wibble: [1, 2, 3])
}

func computeInt() -> Int {
  return 42
}

func triggerUseLazyTestAutoclosure() {
  _ = UseLazy(bar: computeInt())

  // triggerUseLazyTestAutoclosure()
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers29triggerUseLazyTestAutoclosureyyF

  // computeInt() must not get called here
  // CHECK-NOT: // function_ref computeInt()
  // CHECK-NOT: function_ref @$s17property_wrappers10computeIntSiyF : $@convention(thin) () -> Int

  // Rather, an implicit closure is referenced
  // CHECK: // function_ref implicit closure #1 in triggerUseLazyTestAutoclosure()
  // CHECK: function_ref @$s17property_wrappers29triggerUseLazyTestAutoclosureyyFSiycfu_ :

  // And the implicit closure calls computeInt()
  // CHECK: sil private [transparent] [ossa] @$s17property_wrappers29triggerUseLazyTestAutoclosureyyFSiycfu_ :
  // CHECK: // function_ref computeInt()
  // CHECK: function_ref @$s17property_wrappers10computeIntSiyF : $@convention(thin) () -> Int
}

@propertyWrapper
struct WrapperWithNonEscapingAutoclosure<V> {
  var wrappedValue: V
  init(wrappedValue: @autoclosure () -> V) {
    self.wrappedValue = wrappedValue()
  }
}

struct UseWrapperWithNonEscapingAutoclosure {
  @WrapperWithNonEscapingAutoclosure var p1: Int
  @WrapperWithNonEscapingAutoclosure var p2: UInt = 10

  // property wrapper backing initializer of UseWrapperWithNonEscapingAutoclosure.p1
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers36UseWrapperWithNonEscapingAutoclosureV2p1SivpfP : $@convention(thin) (@owned @noescape @callee_guaranteed () -> Int) -> WrapperWithNonEscapingAutoclosure<Int>

  // property wrapper backing initializer of UseWrapperWithNonEscapingAutoclosure.p2
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers36UseWrapperWithNonEscapingAutoclosureV2p2SuvpfP : $@convention(thin) (@owned @noescape @callee_guaranteed () -> UInt) -> WrapperWithNonEscapingAutoclosure<UInt>

  // variable initialization expression of UseWrapperWithNonEscapingAutoclosure._p2
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers36UseWrapperWithNonEscapingAutoclosureV3_p233_F728088E0028E14D18C6A10CF68512E8LLAA0defgH0VySuGvpfi : $@convention(thin) () -> @owned @callee_guaranteed () -> UInt
  // CHECK: return %1 : $@callee_guaranteed () -> UInt

  // default argument 1 of UseWrapperWithNonEscapingAutoclosure.init(p1:p2:)
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers36UseWrapperWithNonEscapingAutoclosureV2p12p2ACSiyXK_SuyXKtcfcfA0_ : $@convention(thin) () -> @owned @callee_guaranteed () -> UInt
  // CHECK: return %1 : $@callee_guaranteed () -> UInt

  // UseWrapperWithNonEscapingAutoclosure.init(p1:p2:)
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers36UseWrapperWithNonEscapingAutoclosureV2p12p2ACSiyXK_SuyXKtcfC : $@convention(method) (@guaranteed @noescape @callee_guaranteed () -> Int, @guaranteed @noescape @callee_guaranteed () -> UInt, @thin UseWrapperWithNonEscapingAutoclosure.Type) -> UseWrapperWithNonEscapingAutoclosure
}

struct UseStatic {
  // CHECK: sil hidden [ossa] @$s17property_wrappers9UseStaticV12staticWibbleSaySiGvgZ
  // CHECK: sil private [global_init] [ossa] @$s17property_wrappers9UseStaticV13_staticWibble33_{{.*}}4LazyOySaySiGGvau
  // CHECK: sil hidden [ossa] @$s17property_wrappers9UseStaticV12staticWibbleSaySiGvsZ
  @Lazy static var staticWibble = [1, 2, 3]
}

extension WrapperWithInitialValue {
  func test() { }
}

class ClassUsingWrapper {
  @WrapperWithInitialValue var x = 0
}

extension ClassUsingWrapper {
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17ClassUsingWrapperC04testcdE01cyAC_tF : $@convention(method) (@guaranteed ClassUsingWrapper, @guaranteed ClassUsingWrapper) -> () {
  func testClassUsingWrapper(c: ClassUsingWrapper) {
    // CHECK: ref_element_addr %1 : $ClassUsingWrapper, #ClassUsingWrapper._x
    self._x.test()
  }
}

// 
@propertyWrapper
struct WrapperWithDefaultInit<T> {
  private var storage: T?

  init() {
    self.storage = nil
  }
  
  init(wrappedValue initialValue: T) {
    self.storage = initialValue
  }

  var wrappedValue: T {
    get { return storage! }
    set { storage = newValue }
  }
}

class UseWrapperWithDefaultInit {
  @WrapperWithDefaultInit var name: String
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers25UseWrapperWithDefaultInitC5_name33_F728088E0028E14D18C6A10CF68512E8LLAA0defG0VySSGvpfi : $@convention(thin) () -> @owned WrapperWithDefaultInit<String>
// CHECK: function_ref @$s17property_wrappers22WrapperWithDefaultInitVACyxGycfC
// CHECK: return {{%.*}} : $WrapperWithDefaultInit<String>

// Property wrapper composition.
@propertyWrapper
struct WrapperA<Value> {
  var wrappedValue: Value
}

@propertyWrapper
struct WrapperB<Value> {
  var wrappedValue: Value
}

@propertyWrapper
struct WrapperC<Value> {
  var wrappedValue: Value?
}

struct CompositionMembers {
  // CompositionMembers.p1.getter
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers18CompositionMembersV2p1SiSgvg : $@convention(method) (@guaranteed CompositionMembers) -> Optional<Int>
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $CompositionMembers):
  // CHECK: [[P1:%.*]] = struct_extract [[SELF]] : $CompositionMembers, #CompositionMembers._p1
  // CHECK: [[P1_VALUE:%.*]] = struct_extract [[P1]] : $WrapperA<WrapperB<WrapperC<Int>>>, #WrapperA.wrappedValue
  // CHECK: [[P1_VALUE2:%.*]] = struct_extract [[P1_VALUE]] : $WrapperB<WrapperC<Int>>, #WrapperB.wrappedValue
  // CHECK: [[P1_VALUE3:%.*]] = struct_extract [[P1_VALUE2]] : $WrapperC<Int>, #WrapperC.wrappedValue
  // CHECK: return [[P1_VALUE3]] : $Optional<Int>
  @WrapperA @WrapperB @WrapperC var p1: Int?
  @WrapperA @WrapperB @WrapperC var p2 = "Hello"

  // variable initialization expression of CompositionMembers.$p2
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers18CompositionMembersV3_p233_{{.*}}8WrapperAVyAA0N1BVyAA0N1CVySSGGGvpfi : $@convention(thin) () -> @owned Optional<String> {
  // CHECK: %0 = string_literal utf8 "Hello"

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers18CompositionMembersV2p12p2ACSiSg_SSSgtcfcfA0_ : $@convention(thin) () -> @owned Optional<String> {
  // CHECK: s17property_wrappers18CompositionMembersV3_p233_{{.*}}8WrapperAVyAA0N1BVyAA0N1CVySSGGGvpfi

}

func testComposition() {
  _ = CompositionMembers(p1: nil)
}

@propertyWrapper
struct WrapperWithAutoclosure<V> {
  var wrappedValue: V
  init(wrappedValue: @autoclosure @escaping () -> V) {
    self.wrappedValue = wrappedValue()
  }
}

struct CompositionWithAutoclosure {
  @WrapperA @WrapperB @WrapperWithAutoclosure var p1: Int
  @WrapperA @WrapperWithAutoclosure @WrapperB var p2: Int
  @WrapperWithAutoclosure @WrapperA @WrapperB var p3: Int

  // In the memberwise init, only p1 should be a closure - p2 and p3 should be just Int
  // CompositionWithAutoclosure.init(p1:p2:p3:)
  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers26CompositionWithAutoclosureV2p12p22p3ACSiyXA_S2itcfC : $@convention(method) (@owned @callee_guaranteed () -> Int, Int, Int, @thin CompositionWithAutoclosure.Type) -> CompositionWithAutoclosure
}

@propertyWrapper
struct WrapperWithAutoclosureAndExtraArgs<V> {
  var wrappedValue: V
  init(wrappedValue: @autoclosure @escaping () -> V, key: String) {
    self.wrappedValue = wrappedValue()
  }
}

struct UseAutoclosureWrapperWithExtraArgs {
  @WrapperWithAutoclosureAndExtraArgs(key: "")
  var value = 10

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers34UseAutoclosureWrapperWithExtraArgsV5valueSivpfP : $@convention(thin) (@owned @callee_guaranteed () -> Int) -> WrapperWithAutoclosureAndExtraArgs<Int>
}

// Observers with non-default mutatingness.
@propertyWrapper
struct NonMutatingSet<T> {
  private var fixed: T

  var wrappedValue: T {
    get { fixed }
    nonmutating set { }
  }

  init(wrappedValue initialValue: T) {
    fixed = initialValue
  }
}

@propertyWrapper
struct MutatingGet<T> {
  private var fixed: T

  var wrappedValue: T {
    mutating get { fixed }
    set { }
  }

  init(wrappedValue initialValue: T) {
    fixed = initialValue
  }
}

@propertyWrapper
struct MutatingGetNonMutatingSet<T> {
  private var fixed: T

  var wrappedValue: T {
    mutating get { fixed }
    nonmutating set { }
  }

  init(wrappedValue initialValue: T) {
    fixed = initialValue
  }
}

struct ObservingTest {
	// ObservingTest.text.setter
	// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers13ObservingTestV4textSSvs : $@convention(method) (@owned String, @guaranteed ObservingTest) -> ()
	// CHECK: function_ref @$s17property_wrappers14NonMutatingSetV12wrappedValuexvg
  @NonMutatingSet var text: String = "" {
    didSet { }
  }

  @NonMutatingSet var integer: Int = 17 {
    willSet { }
  }

  @MutatingGet var text2: String = "" {
    didSet { }
  }

  @MutatingGet var integer2: Int = 17 {
    willSet { }
  }

  @MutatingGetNonMutatingSet var text3: String = "" {
    didSet { }
  }

  @MutatingGetNonMutatingSet var integer3: Int = 17 {
    willSet { }
  }
}

struct NonObservingTest {
  @NonMutatingSet var text: String = ""
  @MutatingGet var text2: String = ""
  @MutatingGetNonMutatingSet var text3: String = ""
}

class NonObservingClassTest {
  @NonMutatingSet var text: String = ""
  @MutatingGet var text2: String = ""
  @MutatingGetNonMutatingSet var text3: String = ""
}

// Projected value with non-default mutatingness
struct Projection<T> {
  var value: T
  init(of value: T) {
    self.value = value
  }
}

@propertyWrapper
struct WrapperWithStorage<T> {
  private var fixed: T

  var wrappedValue: T {
    get { fixed }
    set { }
  }

  var projectedValue: Projection<T> {
    get { Projection(of: fixed) }
    set { }
  }

  init(wrappedValue: T) {
    fixed = wrappedValue
  }
}

@propertyWrapper
struct MutatingGetWithStorage<T> {
  private var fixed: T

  var wrappedValue: T {
    mutating get { fixed }
    set { }
  }

  var projectedValue: Projection<T> {
    mutating get { Projection(of: fixed) }
    set { }
  }

  init(wrappedValue: T) {
    fixed = wrappedValue
  }
}

@propertyWrapper
struct NonMutatingSetWithStorage<T> {
  private var fixed: T

  var wrappedValue: T {
    get { fixed }
    nonmutating set { }
  }

  var projectedValue: Projection<T> {
    get { Projection(of: fixed) }
    nonmutating set { }
  }

  init(wrappedValue: T) {
    fixed = wrappedValue
  }
}

@propertyWrapper
struct MutatingGetNonMutatingSetWithStorage<T> {
  private var fixed: T

  var wrappedValue: T {
    mutating get { fixed }
    nonmutating set { }
  }

  var projectedValue: Projection<T> {
    mutating get { Projection(of: fixed) }
    nonmutating set { }
  }

  init(wrappedValue: T) {
    fixed = wrappedValue
  }
}

struct ProjectedValueWithNonDefaultMutatingnessTest {
  @WrapperWithStorage var n1: Int = 0
  @NonMutatingSetWithStorage var n2: Int = 0
  @MutatingGetWithStorage var n3: Int = 0
  @MutatingGetNonMutatingSetWithStorage var n4: Int = 0
}

class ProjectedValueWithNonDefaultMutatingnessClassTest {
  @WrapperWithStorage var n1: Int = 0
  @NonMutatingSetWithStorage var n2: Int = 0
  @MutatingGetWithStorage var n3: Int = 0
  @MutatingGetNonMutatingSetWithStorage var n4: Int = 0
}

// Composition with non-default mutatingness
struct CompositionWithNonDefaultMutatingnessTest {

  // Two wrappers, all combinations
  @WrapperA @WrapperB var n1: Int
  @WrapperA @MutatingGet var n2: Int
  @WrapperA @NonMutatingSet var n3: Int
  @WrapperA @MutatingGetNonMutatingSet var n4: Int
  @MutatingGetWithStorage @WrapperB var n5: Int
  @MutatingGetWithStorage @MutatingGet var n6: Int
  @MutatingGetWithStorage @NonMutatingSet var n7: Int
  @MutatingGetWithStorage @MutatingGetNonMutatingSet var n8: Int
  @NonMutatingSetWithStorage @WrapperB var n9: Int
  @NonMutatingSetWithStorage @MutatingGet var n10: Int
  @NonMutatingSetWithStorage @NonMutatingSet var n11: Int
  @NonMutatingSetWithStorage @MutatingGetNonMutatingSet var n12: Int
  @MutatingGetNonMutatingSetWithStorage @WrapperB var n13: Int
  @MutatingGetNonMutatingSetWithStorage @MutatingGet var n14: Int
  @MutatingGetNonMutatingSetWithStorage @NonMutatingSet var n15: Int
  @MutatingGetNonMutatingSetWithStorage @MutatingGetNonMutatingSet var n16: Int

  // Three wrappers, non-standard mutability in the middle
  @WrapperA @MutatingGet @WrapperB var m1: Int
  @WrapperA @NonMutatingSet @WrapperB var m2: Int
  @WrapperA @MutatingGetNonMutatingSet @WrapperB var m3: Int
}

// Tuple initial values.
struct WithTuples {
	// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers10WithTuplesVACycfC : $@convention(method) (@thin WithTuples.Type) -> WithTuples {
	// CHECK: function_ref @$s17property_wrappers10WithTuplesV10_fractions33_F728088E0028E14D18C6A10CF68512E8LLAA07WrapperC12InitialValueVySd_S2dtGvpfi : $@convention(thin) () -> (Double, Double, Double)
	// CHECK: function_ref @$s17property_wrappers10WithTuplesV9fractionsSd_S2dtvpfP : $@convention(thin) (Double, Double, Double) -> WrapperWithInitialValue<(Double, Double, Double)>
  @WrapperWithInitialValue var fractions = (1.3, 0.7, 0.3)

	static func getDefault() -> WithTuples {
		return .init()
	}
}

// Resilience with DI of wrapperValue assignments.
// rdar://problem/52467175
class TestResilientDI {
  @MyPublished var data: Int? = nil

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers15TestResilientDIC11doSomethingyyF : $@convention(method) (@guaranteed TestResilientDI) -> () {
  // CHECK: class_method %0 : $TestResilientDI, #TestResilientDI.data!setter : (TestResilientDI) -> (Int?) -> (), $@convention(method) (Optional<Int>, @guaranteed TestResilientDI) -> ()

  func doSomething() {
    self.data = Int()
  }
}

@propertyWrapper
public struct PublicWrapper<T> {
  public var wrappedValue: T

  public init(value: T) {
    wrappedValue = value
  }
}

@propertyWrapper
public struct PublicWrapperWithStorageValue<T> {
  public var wrappedValue: T

  public init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  public var projectedValue: PublicWrapper<T> {
    return PublicWrapper(value: wrappedValue)
  }
}

public class Container {
  public init() {
  }

// The accessor cannot be serializable/transparent because it accesses an
// internal var.
// CHECK-LABEL: sil [ossa] @$s17property_wrappers9ContainerC10$dontCrashAA13PublicWrapperVySiGvg : $@convention(method) (@guaranteed Container) -> PublicWrapper<Int> {
// CHECK: bb0(%0 : @guaranteed $Container):
// CHECK:   ref_element_addr %0 : $Container, #Container._dontCrash
  @PublicWrapperWithStorageValue(wrappedValue: 0) public var dontCrash : Int {
    willSet {
    }
    didSet {
    }
  }
}

// rdar://problem/54311335
// https://github.com/apple/swift/issues/53704
// Crash due to wrong archetype used in generated SIL
public protocol TestProtocol {}
public class TestClass<T> {
  @WrapperWithInitialValue var value: T

  // CHECK-LABEL: sil [ossa] @$s17property_wrappers9TestClassC5valuexvpfP : $@convention(thin) <T> (@in T) -> @out WrapperWithInitialValue<T>

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers9TestClassC5value8protocolACyxGx_qd__tcAA0C8ProtocolRd__lufc
  // CHECK: [[BACKING_INIT:%.*]] = function_ref @$s17property_wrappers9TestClassC5valuexvpfP : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out WrapperWithInitialValue<τ_0_0>
  // CHECK-NEXT: partial_apply [callee_guaranteed] [[BACKING_INIT]]<T>()
  init<U: TestProtocol>(value: T, protocol: U) {
    self.value = value
  }
}

// Composition with wrappedValue initializers that have default values.
@propertyWrapper
struct Outer<Value> {
  var wrappedValue: Value

  init(a: Int = 17, wrappedValue: Value, s: String = "hello") {
    self.wrappedValue = wrappedValue
  }
}


@propertyWrapper
struct Inner<Value> {
  var wrappedValue: Value

  init(wrappedValue: @autoclosure @escaping () -> Value, d: Double = 3.14159) {
    self.wrappedValue = wrappedValue()
  }
}

struct ComposedInit {
  @Outer @Inner var value: Int

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers12ComposedInitV5valueSivpfP : $@convention(thin) (@owned @callee_guaranteed () -> Int) -> Outer<Inner<Int>> {
  // CHECK: function_ref @$s17property_wrappers5InnerV12wrappedValue1dACyxGxyXA_SdtcfcfA0_
  // CHECK: function_ref @$s17property_wrappers5InnerV12wrappedValue1dACyxGxyXA_SdtcfC
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfcfA_
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfcfA1_
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfC
  init() {
    self.value = 17
  }
}

// rdar://problem/55982409 - crash due to improperly inferred 'final'
@propertyWrapper
public struct MyWrapper<T> {
  public var wrappedValue: T
  public var projectedValue: Self { self }
  public init(wrappedValue: T) { self.wrappedValue = wrappedValue }
}

open class TestMyWrapper {
  public init() {}
  @MyWrapper open var useMyWrapper: Int? = nil
}

// rdar://problem/54352235 - crash due to reference to private backing var
extension UsesMyPublished {
  // CHECK-LABEL: sil hidden [ossa] @$s21property_wrapper_defs15UsesMyPublishedC0A9_wrappersE6setFooyySiF : $@convention(method) (Int, @guaranteed UsesMyPublished) -> ()
  // CHECK: class_method %1 : $UsesMyPublished, #UsesMyPublished.foo!setter
  // CHECK-NOT: assign_by_wrapper
  // CHECK: return
  func setFoo(_ x: Int) {
    foo = x
  }
}

// https://github.com/apple/swift/issues/54010
// Crash due to incorrect lvalue computation

@propertyWrapper
struct StructWrapper<T> {
  var wrappedValue: T
}

@propertyWrapper
class ClassWrapper<T> {
  var wrappedValue: T
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

struct S_54010 {
  @StructWrapper @ClassWrapper var prop: Int

  func foo() {
    prop = 1234
  }
}

// rdar://problem/57545381 - crash due to inconsistent decision about whether
// to initialize a wrapper property with an instance of the wrapper type vs.
// the wrapped type.
@propertyWrapper
class WrappedInt {
  var intValue: Int?

  var wrappedValue: Int? {
    get {
      return intValue
    }
    set {
      intValue = newValue
    }
  }

  init() { }

  init(wrappedValue: Int?) {
    self.wrappedValue = wrappedValue
  }
}

struct WrappedIntContainer {
  // CHECK: sil hidden [ossa] @$s17property_wrappers19WrappedIntContainerV3intAcA0cD0C_tcfcfA_ : $@convention(thin) () -> @owned WrappedInt
  @WrappedInt var int: Int?
}

// rdar://problem/59117087 - crash due to incorrectly taking the "self access"
// path for a static property with a wrapper

struct HasStaticWrapper {
  @StructWrapper static var staticWrapper: Int = 0

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers16HasStaticWrapperV08assignToD4SelfyyFZ : $@convention(method) (@thin HasStaticWrapper.Type) -> () {
  static func assignToStaticSelf() {
    // Make sure we call the setter instead of attempting a direct access,
    // like we would if the property was not static:

    // CHECK: function_ref @$s17property_wrappers16HasStaticWrapperV06staticE0SivsZ : $@convention(method) (Int, @thin HasStaticWrapper.Type) -> ()
    staticWrapper = 1
  }
}

@propertyWrapper
struct ObservedObject<ObjectType : AnyObject > {
  var wrappedValue: ObjectType

  init(defaulted: Int = 17, wrappedValue: ObjectType) {
    self.wrappedValue = wrappedValue
  }
}



// rdar://problem/60600911
// Ensure assign_by_wrapper is emitted for initialization
// of a property wrapper with a nonmutating set. Even though such setters
// take `self` by-value.
@propertyWrapper
struct NonMutatingSetterWrapper<Value> {
    var value: Value
    init(wrappedValue: Value) {
        value = wrappedValue
    }
    var wrappedValue: Value {
        get { value }
        nonmutating set {
            print("  .. nonmutatingSet \(newValue)")
        }
    }
}

struct NonMutatingWrapperTestStruct {
    // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers28NonMutatingWrapperTestStructV3valACSi_tcfC : $@convention(method) (Int, @thin NonMutatingWrapperTestStruct.Type) -> NonMutatingWrapperTestStruct {
    // CHECK: %[[LOAD:[0-9]+]] = load [trivial] %[[SRC:[0-9]+]] : $*NonMutatingWrapperTestStruct
    // CHECK-NEXT: %[[SET_PA:[0-9]+]] = partial_apply [callee_guaranteed] [on_stack] %[[PW_SETTER:[0-9]+]](%[[LOAD]]) : $@convention(method) (Int, NonMutatingWrapperTestStruct) -> ()
    // CHECK-NEXT: assign_by_wrapper %[[SETVAL:[0-9]+]] : $Int to %[[ADDR:[0-9]+]] : $*NonMutatingSetterWrapper<Int>, init %[[INIT_PA:[0-9]+]] : $@callee_guaranteed (Int) -> NonMutatingSetterWrapper<Int>, set %[[SET_PA]] : $@noescape @callee_guaranteed (Int) -> ()
    @NonMutatingSetterWrapper var SomeProp: Int
    init(val: Int) {
        SomeProp = val
    }
}


// https://github.com/apple/swift/issues/54882
// Crash on property with wrapper override that adds observer.

@propertyWrapper
struct BasicIntWrapper {
  var wrappedValue: Int
}

class Someclass {
  @BasicIntWrapper var property: Int = 0
}

class Somesubclass : Someclass {
  override var property: Int {
    // Make sure we don't interact with the property wrapper, we just delegate
    // to the superclass' accessors.
    // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers12SomesubclassC0A0Sivs : $@convention(method) (Int, @guaranteed Somesubclass) -> ()
    // CHECK: bb0([[NEW:%.+]] : $Int, {{%.+}} : @guaranteed $Somesubclass):
    // CHECK:   [[SETTER:%.+]] = function_ref @$s17property_wrappers9SomeclassC0A0Sivs : $@convention(method) (Int, @guaranteed Someclass) -> ()
    // CHECK:   apply [[SETTER]]([[NEW]], {{%.+}}) : $@convention(method) (Int, @guaranteed Someclass) -> ()
    // CHECK:   [[DIDSET:%.+]] = function_ref @$s17property_wrappers12SomesubclassC0A0SivW : $@convention(method) (@guaranteed Somesubclass) -> ()
    // CHECK:   apply [[DIDSET]]({{%.+}}) : $@convention(method) (@guaranteed Somesubclass) -> ()
    didSet {
      print("Subclass")
    }
  }
}

// rdar://problem/58986940 - composition of wrappers with autoclosure
@propertyWrapper
struct Once<Value> {
  enum Storage {
    case initialValue(() -> Value)
    case value(Value)
  }

  var storage: Storage

  init(defaulted: Int = 0, wrappedValue value: @escaping @autoclosure () -> Value) {
    storage = .initialValue(value)
  }

  var wrappedValue: Value { fatalError() }
}

class Model {}

struct TestAutoclosureComposition {
  @Once @ObservedObject var model = Model()
}

// https://github.com/apple/swift/issues/58201

@propertyWrapper
struct BasicComputedIntWrapper {
  var wrappedValue: Int { 0 }
}

struct S_58201 {
  func a() {
    @BasicComputedIntWrapper var b: Int
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers7S_58201V1ayyF : $@convention(method) (S_58201) -> () {
// CHECK: bb0(%0 : $S_58201):
// CHECK-NEXT:  debug_value %0 : $S_58201, let, name "self", argno 1
// CHECK-NEXT:  [[BOX:%.*]] = alloc_box ${ var BasicComputedIntWrapper }, var, name "_b"
// CHECK-NEXT:  [[BOX_LIFETIME:%[^,]+]] = begin_borrow [var_decl] [[BOX]]
// CHECK-NEXT:  [[BOXADDR:%.*]] = project_box [[BOX_LIFETIME]] : ${ var BasicComputedIntWrapper }, 0
// CHECK-NEXT:  [[METATYPE:%.*]] = metatype $@thin BasicComputedIntWrapper.Type
// CHECK-NEXT:  // function_ref BasicComputedIntWrapper.init()
// CHECK-NEXT:  [[DEFAULTVALUE_FN:%.*]] = function_ref @$s17property_wrappers23BasicComputedIntWrapperVACycfC : $@convention(method) (@thin BasicComputedIntWrapper.Type) -> BasicComputedIntWrapper
// CHECK-NEXT:  [[DEFAULTRESULT:%.*]] = apply [[DEFAULTVALUE_FN]]([[METATYPE]]) : $@convention(method) (@thin BasicComputedIntWrapper.Type) -> BasicComputedIntWrapper
// CHECK-NEXT:  store [[DEFAULTRESULT]] to [trivial] [[BOXADDR]] : $*BasicComputedIntWrapper
// CHECK-NEXT:  end_borrow [[BOX_LIFETIME]] : ${ var BasicComputedIntWrapper }
// CHECK-NEXT:  destroy_value [[BOX]] : ${ var BasicComputedIntWrapper }
// CHECK-NEXT:  [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:  return [[TUPLE]] : $()
// CHECK-NEXT:  } // end sil function '$s17property_wrappers7S_58201V1ayyF'

// CHECK-LABEL: sil_vtable ClassUsingWrapper {
// CHECK-NEXT:  #ClassUsingWrapper.x!getter: (ClassUsingWrapper) -> () -> Int : @$s17property_wrappers17ClassUsingWrapperC1xSivg   // ClassUsingWrapper.x.getter
// CHECK-NEXT:  #ClassUsingWrapper.x!setter: (ClassUsingWrapper) -> (Int) -> () : @$s17property_wrappers17ClassUsingWrapperC1xSivs // ClassUsingWrapper.x.setter
// CHECK-NEXT:  #ClassUsingWrapper.x!modify: (ClassUsingWrapper) -> () -> () : @$s17property_wrappers17ClassUsingWrapperC1xSivM    // ClassUsingWrapper.x.modify
// CHECK-NEXT:  #ClassUsingWrapper.init!allocator: (ClassUsingWrapper.Type) -> () -> ClassUsingWrapper : @$s17property_wrappers17ClassUsingWrapperCACycfC
// CHECK-NEXT: #ClassUsingWrapper.deinit!deallocator: @$s17property_wrappers17ClassUsingWrapperCfD
// CHECK-NEXT:  }

// CHECK-LABEL: sil_vtable [serialized] TestMyWrapper
// CHECK: #TestMyWrapper.$useMyWrapper!getter

@propertyWrapper
struct AutoclosureWrapper<T> {
  init(wrappedValue: @autoclosure () -> T) {
    self.wrappedValue = wrappedValue()
  }
  var wrappedValue: T
}

struct TestReabstractableWrappedValue<T1> {
  struct S<T2> { }

  @AutoclosureWrapper var v: S<T1> = S()
  init() where T1 == Int { }
}
