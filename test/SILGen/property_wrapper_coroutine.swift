// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@propertyWrapper
struct TestWrapper<ValueType> {
  var wrappedValue: ValueType
}

struct State {
  @TestWrapper var values: [String] = []
}

var state = State()
state.values = Array(repeating: "", count: 20000)

for i in 0..<20000 {
  // CHECK: bb3([[INDEX:%.*]] : $Int):
  // CHECK:  [[BEGIN_ACCESS_MODIFY:%.*]] = begin_access [modify] [dynamic] {{%.*}} : $*State
  // CHECK:  [[REF_MODIFY:%.*]] = function_ref @$s26property_wrapper_coroutine5StateV6valuesSaySSGvM : $@yield_once @convention(method) (@inout State) -> @yields @inout Array<String>
  // CHECK:  ([[RES1:%.*]], {{%.*}}) = begin_apply [[REF_MODIFY]]([[BEGIN_ACCESS_MODIFY]]) : $@yield_once @convention(method) (@inout State) -> @yields @inout Array<String>
  // CHECK:  [[REF_ARRAY_SUBSCRIPT:%.*]] = function_ref @$sSayxSiciM : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
  // CHECK:  ({{%.*}}, {{%.*}}) = begin_apply [[REF_ARRAY_SUBSCRIPT]]<String>([[INDEX]], [[RES1]]) : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
  state.values[i] = String(i)
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s26property_wrapper_coroutine5StateV6valuesSaySSGvM : $@yield_once @convention(method) (@inout State) -> @yields @inout Array<String> {
// CHECK: bb0([[STATE:%.*]] : $*State):
// CHECK:  debug_value_addr [[STATE]] : $*State, var, name "self", argno {{.*}}
// CHECK:  [[BEGIN_ACCESS:%.*]] = begin_access [modify] [unknown] [[STATE]] : $*State
// CHECK:  [[BACKING_ADDR:%.*]] = struct_element_addr [[BEGIN_ACCESS]] : $*State, #State._values
// CHECK:  [[VALUE_ADDR:%.*]] = struct_element_addr [[BACKING_ADDR]] : $*TestWrapper<Array<String>>, #TestWrapper.wrappedValue
// CHECK:  yield [[VALUE_ADDR]] : $*Array<String>, resume bb1, unwind bb2
//
// CHECK: bb1:
// CHECK:  end_access [[BEGIN_ACCESS]] : $*State
// CHECK:  [[RETURN:%.*]] = tuple ()
// CHECK:  return [[RETURN]] : $()
//
// CHECK: bb2:
// CHECK:  end_access [[BEGIN_ACCESS]] : $*State
// CHECK:  unwind
// CHECK-END: }
