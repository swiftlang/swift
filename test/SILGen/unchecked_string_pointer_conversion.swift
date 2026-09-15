// RUN: %target-swift-emit-silgen-ossa -o /dev/null -disable-availability-checking %s
// RUN: %target-swift-emit-silgen -disable-availability-checking %s | %FileCheck %s

func takesConstPointer(_ x: UnsafePointer<UInt16>) {}
func takesConstRawPointer(_ x: UnsafeRawPointer) {}

// CHECK-LABEL: sil hidden [ossa] @$s35unchecked_string_pointer_conversion0A11StringToPtryys09UncheckedE0Vys6UInt16VGF
func uncheckedStringToPtr(_ s: UncheckedString<UInt16>) {
  takesConstPointer(s)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss45_convertConstUncheckedStringToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT]]<UInt16, UnsafePointer<UInt16>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] on [[OWNER]]
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @$s35unchecked_string_pointer_conversion17takesConstPointeryySPys6UInt16VGF
  // CHECK: apply [[TAKES_CONST_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(s)
  // CHECK: [[CONVERT2:%.*]] = function_ref @$ss45_convertConstUncheckedStringToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER2:%.*]] = apply [[CONVERT2]]<UInt16, UnsafeRawPointer>([[POINTER_BUF2:%[0-9]*]],
  // CHECK: [[POINTER2:%.*]] = load [trivial] [[POINTER_BUF2]]
  // CHECK: [[DEPENDENT2:%.*]] = mark_dependence [[POINTER2]] on [[OWNER2]]
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s35unchecked_string_pointer_conversion20takesConstRawPointeryySVF
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[DEPENDENT2]])
  // CHECK: destroy_value [[OWNER2]]
}
