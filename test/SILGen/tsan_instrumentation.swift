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
// CHECK:  [[VALUE_BUFFER:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
// CHECK:  [[TEMPORARY:%.*]] = alloc_stack $Int
// CHECK:  [[BORROWED_CLASS:%.*]] = begin_borrow [[LOADED_CLASS]] : $MyClass
// CHECK:  [[TEMPORARY_RAW:%.*]] = address_to_pointer [[TEMPORARY]] : $*Int to $Builtin.RawPointer
// CHECK:  {{%.*}} = builtin "tsanInoutAccess"([[VALUE_BUFFER]] : $*Builtin.UnsafeValueBuffer) : $()
// CHECK:  [[MATERIALIZE_FOR_SET:%.*]] = class_method [[BORROWED_CLASS]] : $MyClass, #MyClass.storedProperty!materializeForSet.1 : (MyClass) -> (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer) -> (Builtin.RawPointer, Builtin.RawPointer?), $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed MyClass) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK:  [[MATERIALIZE_FOR_SET_TUPLE:%.*]] = apply [[MATERIALIZE_FOR_SET]]([[TEMPORARY_RAW]], [[VALUE_BUFFER]], [[BORROWED_CLASS]]) : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed MyClass) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>)
// CHECK:  [[TEMPORARY_BUFFER:%.*]] = tuple_extract [[MATERIALIZE_FOR_SET_TUPLE]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>), 0
// CHECK:  [[OPTIONAL_CALLBACK:%.*]] = tuple_extract [[MATERIALIZE_FOR_SET_TUPLE]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>), 1
// CHECK:  [[BUFFER_ADDRESS:%.*]] = pointer_to_address [[TEMPORARY_BUFFER]] : $Builtin.RawPointer to [strict] $*Int
// CHECK:  [[BUFFER_ADDRESS_DEPENDENCE:%.*]] = mark_dependence [[BUFFER_ADDRESS]] : $*Int on [[LOADED_CLASS]] : $MyClass
// CHECK:  end_borrow [[BORROWED_CLASS]] from [[LOADED_CLASS]] : $MyClass, $MyClass
// CHECK:  [[BUFFER_ADDRESS_ACCESS:%.*]] = begin_access [modify] [unsafe] [[BUFFER_ADDRESS_DEPENDENCE]]
// CHECK:  {{%.*}} builtin "tsanInoutAccess"([[BUFFER_ADDRESS_ACCESS]] : $*Int) : $()
// CHECK:  [[TAKES_INOUT_FUNC:%.*]] = function_ref @$S20tsan_instrumentation10takesInoutyySizF : $@convention(thin) (@inout Int) -> ()
// CHECK:  {{%.*}} apply [[TAKES_INOUT_FUNC]]([[BUFFER_ADDRESS_ACCESS]]) : $@convention(thin) (@inout Int) -> ()
func inoutGlobalClassStoredProperty() {
  // This generates two TSan inout instrumentations. One for the value
  // buffer that is passed inout to materializeForSet and one for the
  // temporary buffer passed to takesInout().
  takesInout(&gClass.storedProperty)
}
