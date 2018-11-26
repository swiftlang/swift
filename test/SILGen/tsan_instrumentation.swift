// RUN: %target-swift-emit-silgen -sanitize=thread %s | %FileCheck %s
// REQUIRES: tsan_runtime

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

// CHECK-LABEL: sil hidden @$S20tsan_instrumentation17inoutGlobalStructyyF : $@convention(thin) () -> () {
// CHECK:  [[GLOBAL_ADDR:%.*]] = global_addr @$S20tsan_instrumentation7gStructAA02MyC0Vvp : $*MyStruct
// CHECK:  [[WRITE:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]] : $*MyStruct
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[WRITE]] : $*MyStruct) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$S20tsan_instrumentation10takesInoutyyAA8MyStructVzF : $@convention(thin) (@inout MyStruct) -> ()
// CHECK:  {{%.*}} = apply [[TAKES_INOUT_FUNC]]([[WRITE]]) : $@convention(thin) (@inout MyStruct) -> ()
func inoutGlobalStruct() {
  takesInout(&gStruct)
}


// CHECK-LABEL: sil hidden @$S20tsan_instrumentation31inoutGlobalStructStoredPropertyyyF : $@convention(thin) () -> () {
// CHECK:  [[GLOBAL_ADDR:%.*]] = global_addr @$S20tsan_instrumentation7gStructAA02MyC0Vvp : $*MyStruct
// CHECK:  [[WRITE:%.*]] = begin_access [modify] [dynamic] [[GLOBAL_ADDR]] : $*MyStruct
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[WRITE]] : $*MyStruct) : $()
// CHECK:  [[ELEMENT_ADDR:%.*]] = struct_element_addr [[WRITE]] : $*MyStruct, #MyStruct.storedProperty
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[ELEMENT_ADDR]] : $*Int) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$S20tsan_instrumentation10takesInoutyySizF : $@convention(thin) (@inout Int) -> ()
// CHECK:  {{%.*}} = apply [[TAKES_INOUT_FUNC]]([[ELEMENT_ADDR]]) : $@convention(thin) (@inout Int) -> ()
func inoutGlobalStructStoredProperty() {
  // This should generate two TSan inout instrumentations; one for the address
  // of the global and one for the address of the struct stored property.
  takesInout(&gStruct.storedProperty)
}

// CHECK-LABEL: sil hidden @$S20tsan_instrumentation30inoutGlobalClassStoredPropertyyyF : $@convention(thin) () -> () {
// CHECK:  [[GLOBAL_ADDR:%.*]] = global_addr @$S20tsan_instrumentation6gClassAA02MyC0Cvp : $*MyClass
// CHECK:  [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBAL_ADDR]] : $*MyClass
// CHECK:  [[LOADED_CLASS:%.*]] = load [copy] [[READ]] : $*MyClass
// CHECK:  end_access [[READ]]
// CHECK:  [[MODIFY:%.*]] = class_method [[LOADED_CLASS]] : $MyClass, #MyClass.storedProperty!modify.1 :
// CHECK:  ([[BUFFER_ADDRESS:%.*]], [[TOKEN:%.*]]) = begin_apply [[MODIFY]]([[LOADED_CLASS]]) : $@yield_once @convention(method) (@guaranteed MyClass) -> @yields @inout Int
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[BUFFER_ADDRESS]] : $*Int) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$S20tsan_instrumentation10takesInoutyySizF : $@convention(thin) (@inout Int) -> ()
// CHECK:  {{%.*}} apply [[TAKES_INOUT_FUNC]]([[BUFFER_ADDRESS]]) : $@convention(thin) (@inout Int) -> ()
// CHECK:  end_apply [[TOKEN]]
// CHECK:  destroy_value [[LOADED_CLASS]]
func inoutGlobalClassStoredProperty() {
  // This generates two TSan inout instrumentations. One for the value
  // buffer that is passed inout to materializeForSet and one for the
  // temporary buffer passed to takesInout().
  takesInout(&gClass.storedProperty)
}
