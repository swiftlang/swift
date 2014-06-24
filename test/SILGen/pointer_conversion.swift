// RUN: %swift -enable-pointer-conversions -emit-silgen %s | FileCheck %s

func takesMutablePointer(x: UnsafePointer<Int>) {}
func takesConstPointer(x: ConstUnsafePointer<Int>) {}
func takesMutableVoidPointer(x: UnsafePointer<Void>) {}
func takesConstVoidPointer(x: ConstUnsafePointer<Void>) {}

// CHECK-LABEL: sil @_TF18pointer_conversion16pointerToPointerFTGVSs13UnsafePointerSi_GVSs18ConstUnsafePointerSi__T_
// CHECK: bb0([[MP:%.*]] : $UnsafePointer<Int>, [[CP:%.*]] : $ConstUnsafePointer<Int>):
func pointerToPointer(mp: UnsafePointer<Int>,
                      cp: ConstUnsafePointer<Int>) {
  // There should be no conversion here
  takesMutablePointer(mp)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointerFGVSs13UnsafePointerSi_T_
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[MP]])

  takesMutableVoidPointer(mp)
  // CHECK: [[TAKES_MUTABLE_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion23takesMutableVoidPointerFGVSs13UnsafePointerT__T_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs32_convertPointerToPointerArgumentUSs8_Pointer_S___FQ_Q0_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<Int>, UnsafePointer<Void>>
  // CHECK: apply [[TAKES_MUTABLE_VOID_POINTER]]

  takesConstPointer(mp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_TF18pointer_conversion17takesConstPointerFGVSs18ConstUnsafePointerSi_T_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs32_convertPointerToPointerArgumentUSs8_Pointer_S___FQ_Q0_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<Int>, ConstUnsafePointer<Int>>
  // CHECK: apply [[TAKES_CONST_POINTER]]

  takesConstVoidPointer(mp)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion21takesConstVoidPointerFGVSs18ConstUnsafePointerT__T_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs32_convertPointerToPointerArgumentUSs8_Pointer_S___FQ_Q0_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<Int>, ConstUnsafePointer<Void>>
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]
}

// CHECK-LABEL: sil @_TF18pointer_conversion14arrayToPointerFT_T_
func arrayToPointer() {
  var ints = [1,2,3]

  takesMutablePointer(&ints)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointerFGVSs13UnsafePointerSi_T_
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_TFSs37_convertMutableArrayToPointerArgumentU_Ss8_Pointer__FRGSaQ__TGSqPSs9AnyObject__Q0__
  // CHECK: apply [transparent] [[CONVERT_MUTABLE]]<Int, UnsafePointer<Int>>([[TUPLE_BUF:%.*]]#1,
  // CHECK: [[TUPLE:%.*]] = load [[TUPLE_BUF]]#1
  // CHECK: [[OWNER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 0
  // CHECK: [[POINTER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 1
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[POINTER]])
  // CHECK: release_value [[OWNER]]

  takesConstPointer(ints)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_TF18pointer_conversion17takesConstPointerFGVSs18ConstUnsafePointerSi_T_
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_TFSs35_convertConstArrayToPointerArgumentU_Ss8_Pointer__FGSaQ__TGSqPSs9AnyObject__Q0__
  // CHECK: apply [transparent] [[CONVERT_CONST]]<Int, ConstUnsafePointer<Int>>([[TUPLE_BUF:%.*]]#1,
  // CHECK: [[TUPLE:%.*]] = load [[TUPLE_BUF]]#1
  // CHECK: [[OWNER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 0
  // CHECK: [[POINTER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 1
  // CHECK: apply [[TAKES_CONST_POINTER]]([[POINTER]])
  // CHECK: release_value [[OWNER]]
}
