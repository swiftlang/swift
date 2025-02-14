// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -primary-file %s | %FileCheck %s

// 1. Make sure the wrapped property setter calls the observers
// 2. Make sure the synthesized _modify coroutine calls the wrapped property setter

@propertyWrapper 
struct Foo {
  private var _storage: [Int] = []

  init(wrappedValue value: [Int]) {
    self._storage = value
  }

  var wrappedValue: [Int] {
    get { _storage }
    set { _storage = newValue }
  }
}

class Bar {
  @Foo var someArray = [1, 2, 3] {
    willSet {}
    didSet {}
  }
}

// Bar.someArray.setter

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers3BarC9someArraySaySiGvs : $@convention(method) (@owned Array<Int>, @guaranteed Bar) -> () {
// CHECK: bb0([[VALUE:%.*]] : @owned $Array<Int>, [[BAR:%.*]] : @guaranteed $Bar):

// CHECK: [[WILLSET:%.*]] = function_ref @$s26property_wrapper_observers3BarC9someArraySaySiGvw : $@convention(method) (@guaranteed Array<Int>, @guaranteed Bar) -> ()
// CHECK-NEXT: [[RESULT_WS:%.*]] = apply [[WILLSET]](%{{[0-9]+}}, [[BAR]]) : $@convention(method) (@guaranteed Array<Int>, @guaranteed Bar) -> ()

// CHECK: [[WRAPPED_VALUE_SETTER:%.*]] = function_ref @$s26property_wrapper_observers3FooV12wrappedValueSaySiGvs : $@convention(method) (@owned Array<Int>, @inout Foo) -> ()
// CHECK-NEXT: [[RESULT_WVS:%.*]] = apply [[WRAPPED_VALUE_SETTER]](%{{[0-9]+}}, %{{[0-9]+}}) : $@convention(method) (@owned Array<Int>, @inout Foo) -> ()

// CHECK: [[DIDSET:%.*]] = function_ref @$s26property_wrapper_observers3BarC9someArraySaySiGvW : $@convention(method) (@guaranteed Bar) -> ()
// CHECK-NEXT: [[RESULT_DS:%.*]] = apply [[DIDSET]]([[BAR]]) : $@convention(method) (@guaranteed Bar) -> ()
// CHECK: }

// Bar.someArray.modify

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers3BarC9someArraySaySiGvM : $@yield_once @convention(method) (@guaranteed Bar) -> @yields @inout Array<Int> {
// CHECK: bb0([[BAR:%.*]] : @guaranteed $Bar):
// CHECK-NEXT: debug_value [[BAR]] : $Bar, let, name "self", argno 1
// CHECK-NEXT: [[ALLOC_STACK:%.*]] = alloc_stack $Array<Int>
// CHECK-NEXT:  // function_ref Bar.someArray.getter
// CHECK-NEXT: [[GETTER:%.*]] = function_ref @$s26property_wrapper_observers3BarC9someArraySaySiGvg : $@convention(method) (@guaranteed Bar) -> @owned Array<Int>
// CHECK-NEXT:  [[RESULT:%.*]] = apply [[GETTER]]([[BAR]]) : $@convention(method) (@guaranteed Bar) -> @owned Array<Int>
// CHECK-NEXT:  store [[RESULT]] to [init] [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  yield [[ALLOC_STACK]] : $*Array<Int>, resume bb1, unwind bb2

// CHECK: bb1:
// CHECK-NEXT:  [[VALUE:%.*]] = load [take] [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  // function_ref Bar.someArray.setter
// CHECK-NEXT:  [[SETTER:%.*]] = function_ref @$s26property_wrapper_observers3BarC9someArraySaySiGvs : $@convention(method) (@owned Array<Int>, @guaranteed Bar) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = apply [[SETTER]]([[VALUE]], [[BAR]]) : $@convention(method) (@owned Array<Int>, @guaranteed Bar) -> ()
// CHECK-NEXT:  dealloc_stack [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  [[TUPLE:%.*]] = tuple ()
// CHECK-NEXT:  return [[TUPLE]] : $()

// CHECK: bb2:
// CHECK-NEXT:  [[NEWVALUE:%.*]] = load [copy] [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  // function_ref Bar.someArray.setter
// CHECK-NEXT:  [[SETTER:%.*]] = function_ref @$s26property_wrapper_observers3BarC9someArraySaySiGvs : $@convention(method) (@owned Array<Int>, @guaranteed Bar) -> ()
// CHECK-NEXT:  [[RESULT:%.*]] = apply [[SETTER]]([[NEWVALUE]], [[BAR]]) : $@convention(method) (@owned Array<Int>, @guaranteed Bar) -> ()
// CHECK-NEXT:  destroy_addr [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  dealloc_stack [[ALLOC_STACK]] : $*Array<Int>
// CHECK-NEXT:  unwind
// CHECK-END: }


@propertyWrapper
struct State {
  var wrappedValue: Int {
    get { 0 }
    nonmutating set {}
  }
}

struct MutatingDidSet {
  @State private var value: Int {
    mutating didSet {}
  }

  mutating func test() {
    value = 10
  }
}

// MutatingDidSet.value.setter
// CHECK-LABEL: sil private [ossa] @$s26property_wrapper_observers14MutatingDidSetV5value33_{{.*}} : $@convention(method) (Int, @inout MutatingDidSet) -> () {
// CHECK: function_ref @$s26property_wrapper_observers5StateV12wrappedValueSivs : $@convention(method) (Int, State) -> ()
// CHECK: function_ref @$s26property_wrapper_observers14MutatingDidSetV5value33_{{.*}} : $@convention(method) (@inout MutatingDidSet) -> ()

struct MutatingWillSet {
  @State private var value: Int {
    mutating willSet {}
  }

  mutating func test() {
    value = 10
  }
}

// MutatingWillSet.value.setter
// CHECK-LABEL: sil private [ossa] @$s26property_wrapper_observers15MutatingWillSetV5value33_{{.*}}Sivs : $@convention(method) (Int, @inout MutatingWillSet) -> () {
// CHECK: function_ref @$s26property_wrapper_observers15MutatingWillSetV5value33_{{.*}}Sivw : $@convention(method) (Int, @inout MutatingWillSet) -> ()
// CHECK: function_ref @$s26property_wrapper_observers5StateV12wrappedValueSivs : $@convention(method) (Int, State) -> ()

@propertyWrapper struct MutatingGetter {
  var wrappedValue: Int {
    mutating get {
      return 3
    }
    nonmutating set {}
  }
}

struct HasMutatingGetter {
  @MutatingGetter var hasDidSet: Int {
    didSet {}
  }

  @MutatingGetter var hasWillSet: Int {
    willSet {}
  }
}

// The didSet causes the setter to become mutating:

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV9hasDidSetSivg : $@convention(method) (@inout HasMutatingGetter) -> Int {
// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV9hasDidSetSivs : $@convention(method) (Int, @inout HasMutatingGetter) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV9hasDidSetSivM : $@yield_once @convention(method) (@inout HasMutatingGetter) -> @yields @inout Int {

// The willSet does not:

// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV10hasWillSetSivg : $@convention(method) (@inout HasMutatingGetter) -> Int {
// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV10hasWillSetSivs : $@convention(method) (Int, HasMutatingGetter) -> () {
// CHECK-LABEL: sil hidden [ossa] @$s26property_wrapper_observers17HasMutatingGetterV10hasWillSetSivM : $@yield_once @convention(method) (@inout HasMutatingGetter) -> @yields @inout Int {
