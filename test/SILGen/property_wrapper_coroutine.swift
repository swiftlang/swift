// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@propertyWrapper
struct TestWrapper<ValueType> {
  var wrappedValue: ValueType
}

struct State {
  @TestWrapper var values: [String] = []
}

protocol StateProtocol {
  @_borrowed var someValues: [String] { get set }
}

struct State1: StateProtocol {
  @TestWrapper var someValues: [String] = []
}

var state = State()
state.values = Array(repeating: "", count: 20000)
state.values[1000] = "foo"

let state1 = State1()
_ = state1.someValues

// >> Check that the subscript assignment uses the _modify coroutine

// CHECK:  {{%.*}} = begin_access [modify] [dynamic] {{%.*}} : $*State
// CHECK:  [[REF_VALUES_MODIFY:%.*]] = function_ref @$s26property_wrapper_coroutine5StateV6valuesSaySSGvM : $@yield_once @convention(method) (@inout State) -> @yields @inout Array<String>
// CHECK:  ([[RES1:%.*]], {{%.*}}) = begin_apply [[REF_VALUES_MODIFY]]({{%.*}}) : $@yield_once @convention(method) (@inout State) -> @yields @inout Array<String>
// CHECK:  [[REF_ARRAY_SUBSCRIPT:%.*]] = function_ref @$sSayxSiciM : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:  ({{%.*}}, {{%.*}}) = begin_apply [[REF_ARRAY_SUBSCRIPT]]<String>({{%.*}}, [[RES1]]) : $@yield_once @convention(method) <τ_0_0> (Int, @inout Array<τ_0_0>) -> @yields @inout τ_0_0

// >> Check that the _modify coroutine is synthesized properly

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

// >> Check that the _read coroutine is synthesized properly

// CHECK-LABEL: sil shared [ossa] @$s26property_wrapper_coroutine6State1V10someValuesSaySSGvr : $@yield_once @convention(method) (@guaranteed State1) -> @yields @guaranteed Array<String> {
// CHECK: bb0([[STATE1:%.*]] : @guaranteed $State1):
// CHECK:  debug_value [[SELF:%.*]] : $State1, let, name "self", argno {{.*}}
// CHECK:  [[EXTRACT_VALUE:%.*]] = struct_extract %0 : $State1, #State1._someValues
// CHECK:  [[COPY_VALUE:%.*]] = copy_value [[EXTRACT_VALUE]] : $TestWrapper<Array<String>>
// CHECK:  [[BEGIN_BORROW:%.*]] = begin_borrow [[COPY_VALUE]] : $TestWrapper<Array<String>>
// CHECK:  [[EXTRACT_WRAPPEDVALUE:%.*]] = struct_extract [[BEGIN_BORROW]] : $TestWrapper<Array<String>>, #TestWrapper.wrappedValue
// CHECK:  yield [[EXTRACT_WRAPPEDVALUE]] : $Array<String>, resume bb1, unwind bb2
//
// CHECK: bb1:
// CHECK:  end_borrow [[BEGIN_BORROW]] : $TestWrapper<Array<String>>
// CHECK:  destroy_value [[COPY_VALUE]] : $TestWrapper<Array<String>>
// CHECK:  [[RETURN:%.*]] = tuple ()
// CHECK:  return [[RETURN]] : $()
//
// CHECK: bb2:
// CHECK:  end_borrow [[BEGIN_BORROW]] : $TestWrapper<Array<String>>
// CHECK:  destroy_value [[COPY_VALUE]] : $TestWrapper<Array<String>>
// CHECK:  unwind
// CHECK-END: }
