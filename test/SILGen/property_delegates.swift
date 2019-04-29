// RUN: %target-swift-frontend -primary-file %s -emit-silgen | %FileCheck %s
// FIXME: switch to %target-swift-emit-silgen once we have syntax tree support

@_propertyDelegate
struct Wrapper<T> {
  var value: T
}

@_propertyDelegate
struct WrapperWithInitialValue<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
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

  @WrapperWithInitialValue(initialValue: 17)
  var z: Int
}

func forceHasMemberwiseInit() {
  _ = HasMemberwiseInit(x: Wrapper(value: true), y: 17, z: WrapperWithInitialValue(initialValue: 42))
  _ = HasMemberwiseInit<Int>(x: Wrapper(value: true))
  _ = HasMemberwiseInit(y: 17)
  _ = HasMemberwiseInit<Int>(z: WrapperWithInitialValue(initialValue: 42))
  _ = HasMemberwiseInit<Int>()
}

  // CHECK: sil_global hidden @$s18property_delegates9UseStaticV13$staticWibbleAA4LazyOySaySiGGvpZ : $Lazy<Array<Int>>

// HasMemberwiseInit.x.setter
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates17HasMemberwiseInitV1xSbvs : $@convention(method) <T where T : DefaultInit> (Bool, @inout HasMemberwiseInit<T>) -> () {
// CHECK: bb0(%0 : $Bool, %1 : $*HasMemberwiseInit<T>):
// CHECK: [[MODIFY_SELF:%.*]] = begin_access [modify] [unknown] %1 : $*HasMemberwiseInit<T>
// CHECK: [[X_BACKING:%.*]] = struct_element_addr [[MODIFY_SELF]] : $*HasMemberwiseInit<T>, #HasMemberwiseInit.$x
// CHECK: [[X_BACKING_VALUE:%.*]] = struct_element_addr [[X_BACKING]] : $*Wrapper<Bool>, #Wrapper.value
// CHECK: assign %0 to [[X_BACKING_VALUE]] : $*Bool
// CHECK: end_access [[MODIFY_SELF]] : $*HasMemberwiseInit<T>

// variable initialization expression of HasMemberwiseInit.$x
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates17HasMemberwiseInitV2$xAA7WrapperVySbGvpfi : $@convention(thin) <T where T : DefaultInit> () -> Wrapper<Bool> {
// CHECK: integer_literal $Builtin.Int1, 0
// CHECK-NOT: return
// CHECK: function_ref @$sSb22_builtinBooleanLiteralSbBi1__tcfC : $@convention(method) (Builtin.Int1, @thin Bool.Type) -> Bool
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates7WrapperV5valueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0> // user: %9
// CHECK: return {{%.*}} : $Wrapper<Bool>

// variable initialization expression of HasMemberwiseInit.$y
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates17HasMemberwiseInitV2$yAA23WrapperWithInitialValueVyxGvpfi : $@convention(thin) <T where T : DefaultInit> () -> @out T {
// CHECK: bb0(%0 : $*T):
// CHECK-NOT: return
// CHECK: witness_method $T, #DefaultInit.init!allocator.1 : <Self where Self : DefaultInit> (Self.Type) -> () -> Self : $@convention(witness_method: DefaultInit) <τ_0_0 where τ_0_0 : DefaultInit> (@thick τ_0_0.Type) -> @out τ_0_0

// variable initialization expression of HasMemberwiseInit.$z
// CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates17HasMemberwiseInitV2$zAA23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {
// CHECK: bb0:
// CHECK-NOT: return
// CHECK: integer_literal $Builtin.IntLiteral, 17
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates23WrapperWithInitialValueV07initialF0ACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin WrapperWithInitialValue<τ_0_0>.Type) -> @out WrapperWithInitialValue<τ_0_0>

// default argument 0 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s18property_delegates17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA_ : $@convention(thin) <T where T : DefaultInit> () -> Wrapper<Bool> 

// default argument 1 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s18property_delegates17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA0_ : $@convention(thin) <T where T : DefaultInit> () -> @out T {

// default argument 2 of HasMemberwiseInit.init(x:y:z:)
// CHECK: sil hidden [ossa] @$s18property_delegates17HasMemberwiseInitV1x1y1zACyxGAA7WrapperVySbG_xAA0F16WithInitialValueVySiGtcfcfA1_ : $@convention(thin) <T where T : DefaultInit> () -> WrapperWithInitialValue<Int> {


// HasMemberwiseInit.init()
// CHECK-LABEL: sil hidden [ossa] @$s18property_delegates17HasMemberwiseInitVACyxGycfC : $@convention(method) <T where T : DefaultInit> (@thin HasMemberwiseInit<T>.Type) -> @out HasMemberwiseInit<T> {

// Initialization of x
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates17HasMemberwiseInitV2$xAA7WrapperVySbGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> Wrapper<Bool>

// Initialization of y
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates17HasMemberwiseInitV2$yAA23WrapperWithInitialValueVyxGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> @out τ_0_0
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates23WrapperWithInitialValueV07initialF0ACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin WrapperWithInitialValue<τ_0_0>.Type) -> @out WrapperWithInitialValue<τ_0_0>

// Initialization of z
// CHECK-NOT: return
// CHECK: function_ref @$s18property_delegates17HasMemberwiseInitV2$zAA23WrapperWithInitialValueVySiGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : DefaultInit> () -> WrapperWithInitialValue<Int>

// CHECK: return

// CHECK-LABEL: sil private [ossa] @$s18property_delegates9HasNestedV1yACyxGSayxG_tc33_
// CHECK: [[STACK_SLOT:%.*]] = alloc_stack $HasNested<T>.PrivateDelegate<Array<T>>
// CHECK: [[METATYPE:%.*]] = metatype $@thin HasNested<T>.PrivateDelegate<Array<T>>.Type
// CHECK: [[ARRAY_STACK_SLOT:%.*]] = alloc_stack $Array<T>
// CHECK: store %0 to [init] [[ARRAY_STACK_SLOT]] : $*Array<T>
// CHECK: [[INIT:%.*]] = function_ref @$s18property_delegates9HasNestedV15PrivateDelegate{{.*}}initialValue
// CHECK: [[DELEGATE_INSTANCE:%.*]] = apply [[INIT]]<T, [T]>([[STACK_SLOT]], [[ARRAY_STACK_SLOT]], [[METATYPE]])
// CHECK: [[DELEGATE_VALUE:%.*]] = load [take] [[STACK_SLOT]] : $*HasNested<T>.PrivateDelegate<Array<T>>
// CHECK: struct $HasNested<T> ([[DELEGATE_VALUE]] : $HasNested<T>.PrivateDelegate<Array<T>>)
struct HasNested<T> {
  @_propertyDelegate
  private struct PrivateDelegate<U> {
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  @PrivateDelegate
  private var y: [T] = []

  static func blah(y: [T]) -> HasNested<T> {
    return HasNested<T>(y: y)
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

struct DelegateWithAccessors {
  @Wrapper
  var x: Int {
    // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates21DelegateWithAccessorsV1xSivg
    // CHECK-NOT: return
    // CHECK: integer_literal $Builtin.IntLiteral, 42
    return 42

    // Synthesized setter
    // CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates21DelegateWithAccessorsV1xSivs : $@convention(method) (Int, @inout DelegateWithAccessors) -> ()
    // CHECK-NOT: return
    // CHECK: struct_element_addr {{%.*}} : $*DelegateWithAccessors, #DelegateWithAccessors.$x
  }

  mutating func test() {
    x = 17
  }
}

func consumeOldValue(_: Int) { }
func consumeNewValue(_: Int) { }

struct DelegateWithDidSetWillSet {
  // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates022DelegateWithDidSetW
  // CHECK: function_ref @$s18property_delegates022DelegateWithDidSetWillF0V1xSivw
  // CHECK: struct_element_addr {{%.*}} : $*DelegateWithDidSetWillSet, #DelegateWithDidSetWillSet.$x
  // CHECK-NEXT: struct_element_addr {{%.*}} : $*Wrapper<Int>, #Wrapper.value
  // CHECK-NEXT: assign %0 to {{%.*}} : $*Int
  // CHECK: function_ref @$s18property_delegates022DelegateWithDidSetWillF0V1xSivW
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

@_propertyDelegate
struct WrapperWithStorageValue<T> {
  var value: T

  var delegateValue: Wrapper<T> {
    return Wrapper(value: value)
  }
}

struct UseWrapperWithStorageValue {
  // UseWrapperWithStorageValue.$x.getter
  // CHECK-LABEL: sil hidden [transparent] [ossa] @$s18property_delegates26UseWrapperWithStorageValueV2$xAA0D0VySiGvg : $@convention(method) (UseWrapperWithStorageValue) -> Wrapper<Int>
  // CHECK-NOT: return
  // CHECK: function_ref @$s18property_delegates23WrapperWithStorageValueV08delegateF0AA0C0VyxGvg
  @WrapperWithStorageValue(value: 17) var x: Int
}

@_propertyDelegate
enum Lazy<Value> {
  case uninitialized(() -> Value)
  case initialized(Value)

  init(initialValue: @autoclosure @escaping () -> Value) {
    self = .uninitialized(initialValue)
  }

  var value: Value {
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

  // CHECK-LABEL: sil hidden [ossa] @$s18property_delegates7UseLazyV3foo3bar6wibbleACyxGSi_xSaySiGtcfC : $@convention(method) <T where T : DefaultInit> (Int, @in T, @owned Array<Int>, @thin UseLazy<T>.Type) -> @out UseLazy<T>
  // CHECK: function_ref @$s18property_delegates7UseLazyV4$fooAA0D0OySiGvpfiSiycfu_ : $@convention(thin) (@owned Int) -> Int
  // CHECK: function_ref @$s18property_delegates4LazyO12initialValueACyxGxyXA_tcfC : $@convention(method) <τ_0_0> (@owned @callee_guaranteed () -> @out τ_0_0, @thin Lazy<τ_0_0>.Type) -> @out Lazy<τ_0_0>
}

struct X { }

func triggerUseLazy() {
  _ = UseLazy<Int>()
  _ = UseLazy<Int>(foo: 17)
  _ = UseLazy(bar: 17)
  _ = UseLazy<Int>(wibble: [1, 2, 3])
}

struct UseStatic {
  // CHECK: sil hidden [transparent] [ossa] @$s18property_delegates9UseStaticV12staticWibbleSaySiGvgZ
  // CHECK: sil hidden [global_init] [ossa] @$s18property_delegates9UseStaticV13$staticWibbleAA4LazyOySaySiGGvau
  // CHECK: sil hidden [transparent] [ossa] @$s18property_delegates9UseStaticV12staticWibbleSaySiGvsZ
  @Lazy static var staticWibble = [1, 2, 3]
}
