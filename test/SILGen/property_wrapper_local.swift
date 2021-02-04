// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  var projectedValue: Wrapper { self }

  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
}

func testLocalWrapper() {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local16testLocalWrapperyyF : $@convention(thin) () -> () {

  @Wrapper var value: Int
  // CHECK: [[A:%.*]] = alloc_box ${ var Wrapper<Int> }, var, name "_value"
  // CHECK: [[W:%.*]] = mark_uninitialized [var] [[A]] : ${ var Wrapper<Int> }
  // CHECK: [[P:%.*]] = project_box [[W]] : ${ var Wrapper<Int> }

  value = 10
  // CHECK: [[I:%.*]] = function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>
  // CHECK: [[IPA:%.*]] = partial_apply [callee_guaranteed] [[I]]() : $@convention(thin) (Int) -> Wrapper<Int>
  // CHECK: [[S:%.*]] = function_ref @$s22property_wrapper_local16testLocalWrapperyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NEXT: [[C:%.*]] = copy_value [[W]] : ${ var Wrapper<Int> }
  // CHECK-NOT: mark_function_escape
  // CHECK-NEXT: [[SPA:%.*]] = partial_apply [callee_guaranteed] [[S]]([[C]]) : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NEXT: assign_by_wrapper {{%.*}} : $Int to [[P]] : $*Wrapper<Int>, init [[IPA]] : $@callee_guaranteed (Int) -> Wrapper<Int>, set [[SPA]] : $@callee_guaranteed (Int) -> ()

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
  // CHECK: function_ref @$s22property_wrapper_local16testInitialValueyyF5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int>

  value = 15
  // CHECK: function_ref @$s22property_wrapper_local16testInitialValueyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NOT: assign_by_wrapper

  value += 5
  // CHECK: function_ref @$sSi2peoiyySiz_SitFZ : $@convention(method) (@inout Int, Int, @thin Int.Type) -> ()
  // CHECK: function_ref @$s22property_wrapper_local16testInitialValueyyF5valueL_Sivs : $@convention(thin) (Int, @guaranteed { var Wrapper<Int> }) -> ()
  // CHECK-NOT: assign_by_wrapper

  // CHECK: return

  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local16testInitialValueyyF5valueL_SivpfP : $@convention(thin) (Int) -> Wrapper<Int> {
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

func testLocalLazy() {
  // CHECK-LABEL: sil hidden [ossa] @$s22property_wrapper_local13testLocalLazyyyF : $@convention(thin) () -> () {

  @Lazy var value = "hello!"
  // CHECK: [[C:%.*]] = function_ref @$s22property_wrapper_local13testLocalLazyyyFSSycfu_SSycfu0_ : $@convention(thin) () -> @owned String
  // CHECK: [[C2:%.*]] = thin_to_thick_function [[C]] : $@convention(thin) () -> @owned String to $@callee_guaranteed () -> @owned String
  // CHECK: [[I:%.*]] = function_ref @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvpfP : $@convention(thin) (@owned @callee_guaranteed () -> @owned String) -> @owned Lazy<String>
  //CHECK: apply [[I]]([[C2]])

  _ = value
  // CHECK: function_ref @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvg : $@convention(thin) (@guaranteed { var Lazy<String> }) -> @owned String


  // property wrapper backing initializer of value #1 in testLocalLazy()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvpfP : $@convention(thin) (@owned @callee_guaranteed () -> @owned String) -> @owned Lazy<String> {

  // getter of value #1 in testLocalLazy()
  // CHECK-LABEL: sil private [ossa] @$s22property_wrapper_local13testLocalLazyyyF5valueL_SSvg : $@convention(thin) (@guaranteed { var Lazy<String> }) -> @owned String {
  // CHECK: function_ref @$s22property_wrapper_local4LazyO12wrappedValuexvg : $@convention(method) <τ_0_0> (@inout Lazy<τ_0_0>) -> @out τ_0_0
}
