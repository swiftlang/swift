// RUN: %swift -parse-as-library -parse-stdlib -emit-silgen %s | FileCheck %s

import Swift

// A type that is convertible from an inout parameter.
struct ArgPointer<T> {
  let p: Builtin.RawPointer
  static func __inout_conversion(inout x: T) -> ArgPointer {
    return ArgPointer(p: Builtin.addressof(&x))
  }
}

func takeIntArgPointer(x: ArgPointer<Int>) {}

// CHECK-LABEL: sil @_TF16inout_conversion6storedFT_T_
// CHECK: [[BOX:%.*]] = alloc_box $Int
// CHECK: [[FN:%.*]] = function_ref @_TF16inout_conversion17takeIntArgPointer
// CHECK: [[CONVERT:%.*]] = function_ref @_TFV16inout_conversion10ArgPointer18__inout_conversionU__fMGS0
// CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]<Int>([[BOX]]#1, {{%.*}})
// CHECK: apply [[FN]]([[CONVERTED]])
func stored() {
  var mutInt = Int()
  takeIntArgPointer(&mutInt)
}

var computedInt: Int {
  get { return 0 }
  set {}
}

// -- Writeback should be delayed until the end of the expression for which
//    an inout conversion function was called.

func foo() {}

// CHECK-LABEL: sil @_TF16inout_conversion8computedFT_T_
// CHECK: [[FOO:%.*]] = function_ref @_TF16inout_conversion3fooFT_T_
// CHECK: [[FN:%.*]] = function_ref @_TF16inout_conversion17takeIntArgPointer
// CHECK: [[CONVERT:%.*]] = function_ref @_TFV16inout_conversion10ArgPointer18__inout_conversionU__fMGS
// CHECK: [[GET:%.*]] = function_ref @_TF16inout_conversiong11computedIntSi
// CHECK: [[GOTTEN:%.*]] = apply [[GET]]()
// CHECK: [[WRITEBACK:%.*]] = alloc_stack $Int
// CHECK: store [[GOTTEN]] to [[WRITEBACK]]
// CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]<Int>([[WRITEBACK]]#1, {{%.*}})
// CHECK: apply [[FN]]([[CONVERTED]])
// CHECK: [[TO_SET:%.*]] = load [[WRITEBACK]]
// CHECK: [[SETTER:%.*]] = function_ref @_TF16inout_conversions11computedIntSi
// CHECK: apply [[SETTER]]([[TO_SET]])
// CHECK: apply [[FOO]]
func computed() {
  foo(takeIntArgPointer(&computedInt))
}

struct WritebackPointer<T> {
  static func __writeback_conversion(inout _: Int)
  -> WritebackPointer {
    return WritebackPointer()
  }
  static func __writeback_conversion_get(x: T) -> Int {
    return reinterpretCast(x)
  }
  static func __writeback_conversion_set(x: Int) -> T {
    return reinterpretCast(x)
  }
}

func takeWritebackPointer(x: WritebackPointer<String>) {}

// CHECK-LABEL: sil @_TF16inout_conversion19writebackConversionFT_T_
func writebackConversion() {
  var x: String = ""
  foo(takeWritebackPointer(&x))
  // CHECK: [[X_ADDR:%.*]] = alloc_box $String
  // CHECK: [[FOO:%.*]] = function_ref @_TF16inout_conversion3fooFT_T_
  // CHECK: [[TAKE_WRITEBACK_POINTER:%.*]] = function_ref @_TF16inout_conversion20takeWritebackPointer
  // CHECK: [[WRITEBACK_CONVERSION:%.*]] = function_ref @_TFV16inout_conversion16WritebackPointer22__writeback_conversionU__fMGS0_Q__FRSiGS0_Q__
  // -- Get the initial writeback value
  // CHECK: [[WRITEBACK_GET:%.*]] = function_ref @_TFV16inout_conversion16WritebackPointer26__writeback_conversion_get
  // CHECK: [[WRITEBACK_VAL:%.*]] = apply [[WRITEBACK_GET]]<String>
  // CHECK: [[WRITEBACK_BUF:%.*]] = alloc_stack $Int
  // CHECK: store [[WRITEBACK_VAL]] to [[WRITEBACK_BUF]]
  // -- Do the lvalue conversion on the writeback pointer
  // CHECK: [[CONVERTED:%.*]] = apply [[WRITEBACK_CONVERSION]]<String>([[WRITEBACK_BUF]]
  // -- Call takeWritebackPointer
  // CHECK: apply [[TAKE_WRITEBACK_POINTER]]([[CONVERTED]])
  // -- Write back to the original lvalue
  // CHECK: [[WRITEBACK_SET:%.*]] = function_ref @_TFV16inout_conversion16WritebackPointer26__writeback_conversion_set
  // CHECK: apply [[WRITEBACK_SET]]<String>([[WRITEBACK_RES_ADDR:%.*]]#1
  // CHECK: [[WRITEBACK_RES:%.*]] = load [[WRITEBACK_RES_ADDR]]
  // CHECK: assign [[WRITEBACK_RES]] to [[X_ADDR]]
  // -- Apply foo() outside of the writeback scope
  // CHECK: apply [[FOO]]
}
