// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  var projectedValue: Wrapper { self }

  // CHECK-LABEl: sil hidden [ossa] @$s22property_wrapper_local7WrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <T> (@in T, @thin Wrapper<T>.Type) -> @out Wrapper<T>
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

func testLocalWrapper() {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local16testLocalWrapperyyF : $@convention(thin) () -> () {

  @Wrapper var value: Int
  // CHECK: [[A:%.*]] = alloc_box ${ var Wrapper<Int> }, var, name "_value"
  // CHECK: [[W:%.*]] = mark_uninitialized [var] [[A]] : ${ var Wrapper<Int> }
  // CHECK: [[WLIFETIME:%.*]] = begin_borrow [var_decl] [[W]]
  // CHECK: [[P:%.*]] = project_box [[WLIFETIME]] : ${ var Wrapper<Int> }

  value = 10
  // CHECK: [[I:%.*]] = function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>
  // CHECK: [[IPA:%.*]] = partial_apply [callee_guaranteed] [[I]]() : $@convention(thin) (Int) -> Wrapper<Int>
  // CHECK: [[S:%.*]] = function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NEXT: [[C:%.*]] = copy_value [[WLIFETIME]] : ${ var Wrapper<Int> }
  // CHECK-NOT: mark_function_escape
  // CHECK-NEXT: [[SPA:%.*]] = partial_apply [callee_guaranteed] [on_stack] [[S]]([[C]]) : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NEXT: assign_by_wrapper {{%.*}} : $Int to [[P]] : $*Wrapper<Int>, init [[IPA]] : $@callee_guaranteed (Int) -> Wrapper<Int>, set [[SPA]] : $@noescape @callee_guaranteed (Int) -> ()

  _ = value
  // CHECK: mark_function_escape [[P]] : $*Wrapper<Int>
  // CHECK-LABEL: function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivg : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Int

  _ = $value
  // CHECK: mark_function_escape [[P]] : $*Wrapper<Int>
  // CHECK-LABEL: function_ref @$s22property_wrapper_local16testLocalWrapperyyF6$valueL_AA0F0VySiGvg : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Wrapper<Int>

  // Check that the wrapped value is materialized to a temporary when used as `inout`
  value += 5
  // CHECK: [[TMP:%.*]] = alloc_stack $Int
  // CHECK: [[GET:%.*]] = function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivg : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Int
  // CHECK: [[VAL:%.*]] = apply [[GET]]({{%.*}}) : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Int
  // CHECK: store [[VAL]] to [trivial] [[TMP]] : $*Int
  // CHECK: [[OP:%.*]] = function_ref @$sSi2peoiyySiz_SitFZ : $@convention(method) (@inout Int, Int, @thin Int.Type) -> ()
  // CHECK: apply [[OP]]({{%.*}}) : $@convention(method) (@inout Int, Int, @thin Int.Type) -> ()
  // CHECK: [[RESULT:%.*]] = load [trivial] [[TMP]] : $*Int
  // CHECK: assign_by_wrapper [[RESULT]] : $Int to [[P]]

  // Check local property wrapper backing initializer and accessors

  // property wrapper backing initializer of value #1 in testLocalWrapper()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local16testLocalWrapperyyF5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int> {

  // getter of $value #1 in testLocalWrapper()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local16testLocalWrapperyyF6$valueL_AA0F0VySiGvg : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Wrapper<Int> {

  // getter of value #1 in testLocalWrapper()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivg : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Int {

  // setter of value #1 in testLocalWrapper()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> () {
}

func testInitialValue() {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local16testInitialValueyyF : $@convention(thin) () -> () {

  @Wrapper var value: Int = 10
  // CHECK: function_ref @$s22property_wrapper_local7WrapperV12wrappedValueACyxGx_tcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin Wrapper<τ_0_0>.Type) -> @out Wrapper<τ_0_0>

  value = 15
  // CHECK: function_ref @$s22property_wrapper_local16testInitialValueyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NOT: assign_by_wrapper

  value += 5
  // CHECK: function_ref @$sSi2peoiyySiz_SitFZ : $@convention(method) (@inout Int, Int, @thin Int.Type) -> ()
  // CHECK: function_ref @$s22property_wrapper_local16testInitialValueyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NOT: assign_by_wrapper

  // CHECK: return
}

@propertyWrapper
enum Lazy<Value> {
  case uninitialized(() -> Value)
  case initialized(Value)

  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local4LazyO12wrappedValueACyxGxyXA_tcfC : $@convention(method) <Value> (@owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <Value>, @thin Lazy<Value>.Type) -> @out Lazy<Value>
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

func testLocalLazy() {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local13testLocalLazyyyF : $@convention(thin) () -> () {

  @Lazy var value = "hello!"
  // CHECK: function_ref @$s22property_wrapper_local13testLocalLazyyyFSSycfu_ :
  // CHECK: [[I:%.*]] = function_ref @$s22property_wrapper_local4LazyO12wrappedValueACyxGxyXA_tcfC : $@convention(method) <τ_0_0> (@owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <τ_0_0>, @thin Lazy<τ_0_0>.Type) -> @out Lazy<τ_0_0>
  //CHECK: apply [[I]]<String>

  _ = value
  // CHECK: function_ref @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvg : $@convention(thin) (@guaranteed { var Lazy<String> }) -> @owned String

  // getter of value #1 in testLocalLazy()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvg : $@convention(thin) (@guaranteed { var Lazy<String> }) -> @owned String {
  // CHECK: function_ref @$s22property_wrapper_local4LazyO12wrappedValuexvg : $@convention(method) <τ_0_0> (@inout Lazy<τ_0_0>) -> @out τ_0_0
}

@propertyWrapper
struct BoundedNumber<T: Numeric & Comparable> {
  private let min:T
  private let max:T
  var value:T

  var wrappedValue: T {
    get { value }
    set {
      if value < min {
        value =  min
      } else if value > max {
        value = max
      }
    }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local13BoundedNumberV12wrappedValue3min3maxACyxGx_xxtcfC : $@convention(method) <T where T : Comparable, T : Numeric> (@in T, @in T, @in T, @thin BoundedNumber<T>.Type) -> @out BoundedNumber<T>
  init(wrappedValue: T, min: T, max: T) {
    self.min = min
    self.max = max

    if wrappedValue < min {
      self.value =  min
    } else if wrappedValue > max {
      self.value = max
    } else {
      self.value = wrappedValue
    }
  }
}

func testLocalReference(count: Int) {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local18testLocalReference5countySi_tF : $@convention(thin) (Int) -> ()

  @BoundedNumber(min: 0, max: count) var value = 10
  // CHECK: function_ref @$s22property_wrapper_local13BoundedNumberV12wrappedValue3min3maxACyxGx_xxtcfC : $@convention(method) <τ_0_0 where τ_0_0 : Comparable, τ_0_0 : Numeric> (@in τ_0_0, @in τ_0_0, @in τ_0_0, @thin BoundedNumber<τ_0_0>.Type) -> @out BoundedNumber<τ_0_0>

  _ = value
  // CHECK: function_ref @$s22property_wrapper_local18testLocalReference5countySi_tF5valueL_Sivg : $@convention(thin) (@guaranteed { var BoundedNumber<Int> }) -> Int

  value = count
  // CHECK: function_ref @$s22property_wrapper_local18testLocalReference5countySi_tF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var BoundedNumber<Int> }) -> ()

  // getter of value #1 in testLocalReference(count:)
  // CHECK: sil private [ossa] @$s22property_wrapper_local18testLocalReference5countySi_tF5valueL_Sivg : $@convention(thin) (@guaranteed { var BoundedNumber<Int> }) -> Int

  // setter of value #1 in testLocalReference(count:)
  // CHECK: sil private [ossa] @$s22property_wrapper_local18testLocalReference5countySi_tF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var BoundedNumber<Int> }) -> ()
}

func takesAutoclosure(_: @autoclosure () -> Int) {}

// CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local12testCapturesyyF : $@convention(thin) () -> ()
func testCaptures() {
  @Wrapper var value = 10
  takesAutoclosure(value)
  // implicit closure #1 in testCaptures()
  // CHECK-LABEL: sil private [transparent] [ossa] @$s22property_wrapper_local12testCapturesyyFSiyXEfu_ : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> Int

  let _: () -> Void = {
    _ = value
    value = 100
  }
  // closure #1 in testCaptures()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local12testCapturesyyFyycfU_ : $@convention(thin) (@guaranteed { var Wrapper<Int> }) -> ()
}

@propertyWrapper
struct DefaultInit {
  var wrappedValue: Int

  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local11DefaultInitVACycfC : $@convention(method) (@thin DefaultInit.Type) -> DefaultInit
  init() {
    self.wrappedValue = 0
  }

  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local11DefaultInitV5valueACSi_tcfC : $@convention(method) (Int, @thin DefaultInit.Type) -> DefaultInit
  init(value: Int) {
    self.wrappedValue = value
  }
}

@propertyWrapper
struct DefaultWrappedValue {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local19DefaultWrappedValueVACycfC : $@convention(method) (@thin DefaultWrappedValue.Type) -> DefaultWrappedValue
  var wrappedValue: Int = 10
}

// CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local20testLocalDefaultInityyF : $@convention(thin) () -> ()
func testLocalDefaultInit() {
  // CHECK: function_ref @$s22property_wrapper_local11DefaultInitVACycfC : $@convention(method) (@thin DefaultInit.Type) -> DefaultInit
  @DefaultInit var x: Int

  // CHECK: function_ref @$s22property_wrapper_local11DefaultInitV5valueACSi_tcfC : $@convention(method) (Int, @thin DefaultInit.Type) -> DefaultInit
  @DefaultInit(value: 10) var z: Int

  // CHECK: function_ref @$s22property_wrapper_local11DefaultInitVACycfC : $@convention(method) (@thin DefaultInit.Type) -> DefaultInit
  @DefaultInit() var y: Int

  // CHECK: function_ref @$s22property_wrapper_local19DefaultWrappedValueVACycfC : $@convention(method) (@thin DefaultWrappedValue.Type) -> DefaultWrappedValue
  @DefaultWrappedValue var w: Int
}
