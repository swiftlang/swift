// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/property_wrapper_defs.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s
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
// CHECK: witness_method $T, #DefaultInit.init!allocator.1 : <Self where Self : DefaultInit> (Self.Type) -> () -> Self : $@convention(witness_method: DefaultInit) <τ_0_0 where τ_0_0 : DefaultInit> (@thick τ_0_0.Type) -> @out τ_0_0

// variable initialization expression of HasMemberwiseInit._z
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s17property_wrappers17HasMemberwiseInitV2_z33_{{.*}}23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {
// CHECK: bb0:
// CHECK-NOT: return
// CHECK: integer_literal $Builtin.IntLiteral, 17
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers23WrapperWithInitialValueV07wrappedF0ACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin WrapperWithInitialValue<τ_0_0>.Type) -> @out WrapperWithInitialValue<τ_0_0>

// default argument 0 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA_ : $@convention(thin) <T where T : DefaultInit> () -> Wrapper<Bool> 

// default argument 1 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA0_ : $@convention(thin) <T where T : DefaultInit> () -> @out T {

// default argument 2 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA1_ : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {


// HasMemberwiseInit.init()
// CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers17HasMemberwiseInitVACyxGycfC : $@convention(method) <T where T : DefaultInit> (@thin HasMemberwiseInit<T>.Type) -> @out HasMemberwiseInit<T> {

// Initialization of x
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_x33_{{.*}}7WrapperVySbGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> Wrapper<Bool>

// Initialization of y
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_y33_{{.*}}23WrapperWithInitialValueVyxGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> @out τ_0_0
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV1yxvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@in τ_0_0) -> @out WrapperWithInitialValue<τ_0_0>

// Initialization of z
// CHECK-NOT: return
// CHECK: function_ref @$s17property_wrappers17HasMemberwiseInitV2_z33_{{.*}}23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> WrapperWithInitialValue<Int>

// CHECK: return

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

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers7UseLazyV3foo3bar6wibbleACyxGSi_xSaySiGtcfC : $@convention(method) <T where T : DefaultInit> (Int, @in T, @owned Array<Int>, @thin UseLazy<T>.Type) -> @out UseLazy<T>
  // CHECK: function_ref @$s17property_wrappers7UseLazyV3fooSivpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (Int) -> @owned Lazy<Int>
  // CHECK: function_ref @$s17property_wrappers7UseLazyV3barxvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@in τ_0_0) -> @out Lazy<τ_0_0>
  // CHECK: function_ref @$s17property_wrappers7UseLazyV6wibbleSaySiGvpfP : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> (@owned Array<Int>) -> @owned Lazy<Array<Int>>
}

struct X { }

func triggerUseLazy() {
  _ = UseLazy<Int>()
  _ = UseLazy<Int>(foo: 17)
  _ = UseLazy(bar: 17)
  _ = UseLazy<Int>(wibble: [1, 2, 3])
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

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers18CompositionMembersV2p12p2ACSiSg_SSSgtcfC : $@convention(method) (Optional<Int>, @owned Optional<String>, @thin CompositionMembers.Type) -> @owned CompositionMembers
  // CHECK: s17property_wrappers18CompositionMembersV3_p233_{{.*}}8WrapperAVyAA0N1BVyAA0N1CVySSGGGvpfi

}

func testComposition() {
  _ = CompositionMembers(p1: nil)
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

	// CHECK: assign_by_wrapper {{%.*}} : $Optional<Int> to {{%.*}} : $*MyPublished<Optional<Int>>, init {{%.*}} : $@callee_guaranteed (Optional<Int>) -> @out MyPublished<Optional<Int>>, set {{%.*}} : $@callee_guaranteed (Optional<Int>) -> ()

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

// SR-11303 / rdar://problem/54311335 - crash due to wrong archetype used in generated SIL
public protocol TestProtocol {}
public class TestClass<T> {
  @WrapperWithInitialValue var value: T

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

  // CHECK-LABEL: sil hidden [ossa] @$s17property_wrappers12ComposedInitV5valueSivpfP : $@convention(thin) (Int) -> Outer<Inner<Int>> {
  // CHECK: function_ref @$s17property_wrappers12ComposedInitV6_value33_F728088E0028E14D18C6A10CF68512E8LLAA5OuterVyAA5InnerVySiGGvpfiSiycfu_
  // CHECK: function_ref @$s17property_wrappers5InnerV12wrappedValue1dACyxGxyXA_SdtcfcfA0_
  // CHECK: function_ref @$s17property_wrappers5InnerV12wrappedValue1dACyxGxyXA_SdtcfC
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfcfA_
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfcfA1_
  // CHECK: function_ref @$s17property_wrappers5OuterV1a12wrappedValue1sACyxGSi_xSStcfC
  init() {
    self.value = 17
  }
}


// CHECK-LABEL: sil_vtable ClassUsingWrapper {
// CHECK-NEXT:  #ClassUsingWrapper.x!getter.1: (ClassUsingWrapper) -> () -> Int : @$s17property_wrappers17ClassUsingWrapperC1xSivg   // ClassUsingWrapper.x.getter
// CHECK-NEXT:  #ClassUsingWrapper.x!setter.1: (ClassUsingWrapper) -> (Int) -> () : @$s17property_wrappers17ClassUsingWrapperC1xSivs // ClassUsingWrapper.x.setter
// CHECK-NEXT:  #ClassUsingWrapper.x!modify.1: (ClassUsingWrapper) -> () -> () : @$s17property_wrappers17ClassUsingWrapperC1xSivM    // ClassUsingWrapper.x.modify
// CHECK-NEXT:  #ClassUsingWrapper.init!allocator.1: (ClassUsingWrapper.Type) -> () -> ClassUsingWrapper : @$s17property_wrappers17ClassUsingWrapperCACycfC
// CHECK-NEXT: #ClassUsingWrapper.deinit!deallocator.1: @$s17property_wrappers17ClassUsingWrapperCfD
// CHECK-NEXT:  }
