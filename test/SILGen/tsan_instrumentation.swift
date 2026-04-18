// REQUIRES: tsan_runtime
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sanitize=thread %s | %FileCheck %s
// RUN: %target-swift-frontend -sanitize=thread -emit-ir -primary-file %s | %FileCheck --check-prefix=CHECK-LLVM-IR %s

// TSan is only supported on 64 bit.
// REQUIRES: PTRSIZE=64

func takesInout(_ p: inout Int) { }
func takesInout(_ p: inout MyStruct) { }


struct MyStruct {
  var storedProperty: Int = 77
}

class MyClass {
  var storedProperty: Int = 22
}

var gStruct = MyStruct()
var gClass = MyClass()

// CHECK-LABEL: sil hidden [ossa] @$s20tsan_instrumentation17inoutGlobalStructyyF : $@convention(thin) () -> () {
// CHECK:  [[GLOBAL_ADDR:%.*]] = global_addr @$s20tsan_instrumentation7gStructAA02MyC0Vvp : $*MyStruct
// CHECK:  [[WRITE:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]] : $*MyStruct
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[WRITE]] : $*MyStruct) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$s20tsan_instrumentation10takesInoutyyAA8MyStructVzF : $@convention(thin) (@inout MyStruct) -> ()
// CHECK:  {{%.*}} = apply [[TAKES_INOUT_FUNC]]([[WRITE]]) : $@convention(thin) (@inout MyStruct) -> ()
func inoutGlobalStruct() {
  takesInout(&gStruct)
}


// CHECK-LABEL: sil hidden [ossa] @$s20tsan_instrumentation31inoutGlobalStructStoredPropertyyyF : $@convention(thin) () -> () {
// CHECK:  [[GLOBAL_ADDR:%.*]] = global_addr @$s20tsan_instrumentation7gStructAA02MyC0Vvp : $*MyStruct
// CHECK:  [[WRITE:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]] : $*MyStruct
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[WRITE]] : $*MyStruct) : $()
// CHECK:  [[ELEMENT_ADDR:%.*]] = struct_element_addr [[WRITE]] : $*MyStruct, #MyStruct.storedProperty
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[ELEMENT_ADDR]] : $*Int) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$s20tsan_instrumentation10takesInoutyySizF : $@convention(thin) (@inout Int) -> ()
// CHECK:  {{%.*}} = apply [[TAKES_INOUT_FUNC]]([[ELEMENT_ADDR]]) : $@convention(thin) (@inout Int) -> ()
func inoutGlobalStructStoredProperty() {
  // This should generate two TSan inout instrumentations; one for the address
  // of the global and one for the address of the struct stored property.
  takesInout(&gStruct.storedProperty)
}

// CHECK-LABEL: sil hidden [ossa] @$s20tsan_instrumentation30inoutGlobalClassStoredPropertyyyF : $@convention(thin) () -> () {
// CHECK-NEXT: bb0:
// CHECK-NEXT:  [[GLOBAL_ADDR:%.*]] = global_addr @$s20tsan_instrumentation6gClassAA02MyC0Cvp : $*MyClass
// CHECK-NEXT:  [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*MyClass
// CHECK-NEXT:  [[LOADED_CLASS:%.*]] = load [copy] [[READ]] : $*MyClass
// CHECK-NEXT:  end_access [[READ]]
// CHECK-NEXT:  [[BORROWED_CLASS:%.*]] = begin_borrow [[LOADED_CLASS]]
// CHECK-NEXT:  [[MODIFY:%.*]] = class_method [[BORROWED_CLASS]] : $MyClass, #MyClass.storedProperty!yielding_mutate :
// CHECK-NEXT:  ([[BUFFER_ADDRESS:%.*]], [[TOKEN:%.*]], [[CORO_ALLOCATION:%.*]]) = begin_apply [[MODIFY]]([[BORROWED_CLASS]]) : $@yield_once_2 @convention(method) (@guaranteed MyClass) -> @yields @inout Int
// CHECK-NEXT:  {{%.*}} = builtin "tsanInoutAccess"([[BUFFER_ADDRESS]] : $*Int) : $()
// CHECK-NEXT:  // function_ref takesInout
// CHECK-NEXT:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$s20tsan_instrumentation10takesInoutyySizF : $@convention(thin) (@inout Int) -> ()
// CHECK-NEXT:  {{%.*}} apply [[TAKES_INOUT_FUNC]]([[BUFFER_ADDRESS]]) : $@convention(thin) (@inout Int) -> ()
// CHECK-NEXT:  end_apply [[TOKEN]]
// CHECK-NEXT:  end_borrow [[BORROWED_CLASS]]
// CHECK-NEXT:  destroy_value [[LOADED_CLASS]]
// CHECK-NEXT:  dealloc_stack [[CORO_ALLOCATION]]
// CHECK-NEXT:  [[VOID_RET:%.*]] = tuple ()
// CHECK-NEXT:  return [[VOID_RET]]

func inoutGlobalClassStoredProperty() {
  // This generates two TSan inout instrumentations. One for the value
  // buffer that is passed inout to materializeForSet and one for the
  // temporary buffer passed to takesInout().
  takesInout(&gClass.storedProperty)
}

// Known-empty types don't have storage, so there is no address
// to pass to the TSan runtime to check for data races on inout accesses.
// In this case, the instrumentation should skip the call to
// __tsan_external_write()

struct ZeroSizedStruct {
  mutating
  func mutate() { }
}

struct NonEmptyStruct {
  var f: Int = 5
  mutating
  func mutate() { }
}

// CHECK-LLVM-IR-LABEL: testNoInstrumentZeroSizedStruct
func testNoInstrumentZeroSizedStruct() {
  var s = ZeroSizedStruct()

// CHECK-LLVM-IR-NOT: tsan_external_write
  s.mutate()
}

func takesInout<T>(_ p: inout T) { }

// CHECK-LLVM-IR-LABEL: testNoInstrumentEmptyTuple
func testNoInstrumentEmptyTuple() {
  var t: Void = ()

// CHECK-LLVM-IR-NOT: tsan_external_write
  takesInout(&t)
}

// CHECK-LLVM-IR-LABEL: testNoInstrumentMutateInoutZeroSizedStruct
func testNoInstrumentMutateInoutZeroSizedStruct(p: inout ZeroSizedStruct) {

// CHECK-LLVM-IR-NOT: tsan_external_write
  p.mutate()
}

// CHECK-LLVM-IR-LABEL: testInstrumentNonEmptyStruct
func testInstrumentNonEmptyStruct() {
  // Make sure we actually instrument accesses to non-empty structs.
  var s = NonEmptyStruct()

// CHECK-LLVM-IR: tsan_external_write
  s.mutate()
}
