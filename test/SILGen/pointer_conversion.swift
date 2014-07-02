// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -emit-silgen -enable-string-pointer-conversion -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

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

// CHECK-LABEL: sil @_TF18pointer_conversion15stringToPointerFSST_ 
func stringToPointer(s: String) {
  takesConstVoidPointer(s)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion21takesConstVoidPointerFGVSs18ConstUnsafePointerT__T_
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_TFSs40_convertConstStringToUTF8PointerArgumentUSs8_Pointer__FSSTGSqPSs9AnyObject__Q__
  // CHECK: apply [transparent] [[CONVERT_STRING]]<ConstUnsafePointer<Void>>([[TUPLE_BUF:%.*]]#1,
  // CHECK: [[TUPLE:%.*]] = load [[TUPLE_BUF]]#1
  // CHECK: [[OWNER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 0
  // CHECK: [[POINTER:%.*]] = tuple_extract [[TUPLE]] : ${{.*}}, 1
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]([[POINTER]])
  // CHECK: release_value [[OWNER]]
}

// CHECK-LABEL: sil @_TF18pointer_conversion14inoutToPointerFT_T_ 
func inoutToPointer() {
  var int = 0
  // CHECK: [[INT:%.*]] = alloc_box $Int
  takesMutablePointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointerFGVSs13UnsafePointerSi_T_
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[INT]]#1
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs30_convertInOutToPointerArgumentUSs8_Pointer__FBpQ_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<Int>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_MUTABLE]]

  var logicalInt: Int {
    get { return 0 }
    set { }
  }
  takesMutablePointer(&logicalInt)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointerFGVSs13UnsafePointerSi_T_
  // CHECK: [[GETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_gL_10logicalIntSi
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs30_convertInOutToPointerArgumentUSs8_Pointer__FBpQ_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<Int>>
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_sL_10logicalIntSi
  // CHECK: apply [[SETTER]]
}

class C {}

func takesPlusOnePointer(x: UnsafePointer<C>) {}
func takesPlusZeroPointer(x: AutoreleasingUnsafePointer<C>) {}
func takesPlusZeroOptionalPointer(x: AutoreleasingUnsafePointer<C?>) {}

// CHECK-LABEL: sil @_TF18pointer_conversion19classInoutToPointerFT_T_
func classInoutToPointer() {
  var c = C()
  // CHECK: [[VAR:%.*]] = alloc_box $C
  takesPlusOnePointer(&c)
  // CHECK: [[TAKES_PLUS_ONE:%.*]] = function_ref @_TF18pointer_conversion19takesPlusOnePointerFGVSs13UnsafePointerCS_1C_T_
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[INT]]#1
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs30_convertInOutToPointerArgumentUSs8_Pointer__FBpQ_
  // CHECK: apply [transparent] [[CONVERT]]<UnsafePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_PLUS_ONE]]

  takesPlusZeroPointer(&c)
  // CHECK: [[TAKES_PLUS_ZERO:%.*]] = function_ref @_TF18pointer_conversion20takesPlusZeroPointerFGVSs26AutoreleasingUnsafePointerCS_1C_T_
  // CHECK: [[OWNED:%.*]] = load [[VAR]]
  // CHECK: [[UNOWNED:%.*]] = ref_to_unmanaged [[OWNED]]
  // CHECK: [[WRITEBACK:%.*]] = alloc_stack $@sil_unmanaged C
  // CHECK: store [[UNOWNED]] to [[WRITEBACK]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITEBACK]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFSs30_convertInOutToPointerArgumentUSs8_Pointer__FBpQ_
  // CHECK: apply [transparent] [[CONVERT]]<AutoreleasingUnsafePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_PLUS_ZERO]]
  // CHECK: [[UNOWNED_OUT:%.*]] = load [[WRITEBACK]]
  // CHECK: [[OWNED_OUT:%.*]] = unmanaged_to_ref [[UNOWNED_OUT]]
  // CHECK: retain_value [[OWNED_OUT]]
  // CHECK: assign [[OWNED_OUT]] to [[VAR]]

  var cq: C? = C()
  takesPlusZeroOptionalPointer(&cq)
}

// Check that pointer types don't bridge anymore.
@objc class ObjCMethodBridging {
  // CHECK-LABEL: sil @_TToFC18pointer_conversion18ObjCMethodBridging11pointerArgsfS0_FTGVSs13UnsafePointerSi_1yGVSs18ConstUnsafePointerSi_1zGVSs26AutoreleasingUnsafePointerS0___T_ : $@cc(objc_method) @thin (UnsafePointer<Int>, ConstUnsafePointer<Int>, AutoreleasingUnsafePointer<ObjCMethodBridging>, ObjCMethodBridging)
  @objc func pointerArgs(x: UnsafePointer<Int>,
                         y: ConstUnsafePointer<Int>,
                         z: AutoreleasingUnsafePointer<ObjCMethodBridging>) {}
}
