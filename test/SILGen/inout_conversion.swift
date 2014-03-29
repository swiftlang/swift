// RUN: %swift -parse-as-library -parse-stdlib -emit-silgen -verify %s | FileCheck %s

import Swift

// A type that is convertible from an inout parameter.
struct ArgPointer<T>: BuiltinInOutAddressConvertible {
  typealias _InOutType = T
  static func _convertFromInOutAddress(addr: Builtin.RawPointer) -> ArgPointer {
    return ArgPointer()
  }
}

struct ConcreteInputArgPointer<T>: BuiltinInOutAddressConvertible {
  typealias _InOutType = Int -> T
  static func _convertFromInOutAddress(addr: Builtin.RawPointer)
  -> ConcreteInputArgPointer {
    return ConcreteInputArgPointer()
  }
}

func takeIntArgPointer(x: ArgPointer<Int>) {}
func takeFnArgPointer(x: ArgPointer<Int -> Int>) {}
func takeConcreteInputFnArgPointer(x: ConcreteInputArgPointer<Int>) {}

// CHECK-LABEL: sil @_TF16inout_conversion6storedFT_T_
// CHECK: [[BOX:%.*]] = alloc_box $Int
// CHECK: [[FN:%.*]] = function_ref @_TF16inout_conversion17takeIntArgPointerFT1xGVS_10ArgPointerSi__T_
// CHECK: [[CONVERT:%.*]] = function_ref @_TFV16inout_conversion10ArgPointer24_convertFromInOutAddressU__fMGS0_Q__FT4addrBp_GS0_Q__
// CHECK: [[PTR:%.*]] = address_to_pointer [[BOX]]#1
// CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]<Int>([[PTR]], {{%.*}})
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
// CHECK: [[FN:%.*]] = function_ref @_TF16inout_conversion17takeIntArgPointerFT1xGVS_10ArgPointerSi__T_
// CHECK: [[CONVERT:%.*]] = function_ref @_TFV16inout_conversion10ArgPointer24_convertFromInOutAddressU__fMGS0_Q__FT4addrBp_GS0_Q__
// CHECK: [[GET:%.*]] = function_ref @_TF16inout_conversiong11computedIntSi
// CHECK: [[GOTTEN:%.*]] = apply [[GET]]()
// CHECK: [[WRITEBACK:%.*]] = alloc_stack $Int
// CHECK: store [[GOTTEN]] to [[WRITEBACK]]
// CHECK: [[PTR:%.*]] = address_to_pointer [[WRITEBACK]]#1
// CHECK: [[CONVERTED:%.*]] = apply [[CONVERT]]<Int>([[PTR]], {{%.*}})
// CHECK: apply [[FN]]([[CONVERTED]])
// CHECK: [[TO_SET:%.*]] = load [[WRITEBACK]]
// CHECK: [[SETTER:%.*]] = function_ref @_TF16inout_conversions11computedIntSi
// CHECK: apply [[SETTER]]([[TO_SET]])
// CHECK: apply [[FOO]]
func computed() {
  foo(takeIntArgPointer(&computedInt))
}

// -- TODO: Values are reabstracted to the abstraction level of the _InOutType
//    of the type we're converting to.
func functionAbstraction() {
  var fn: Int -> Int = { $0 }
  takeFnArgPointer(&fn) // expected-error{{not implemented}}
  takeConcreteInputFnArgPointer(&fn) //expected-error{{not implemented}}
}
