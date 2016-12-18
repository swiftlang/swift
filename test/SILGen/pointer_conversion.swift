// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Foundation

func takesMutablePointer(_ x: UnsafeMutablePointer<Int>) {}
func takesConstPointer(_ x: UnsafePointer<Int>) {}
func takesMutableVoidPointer(_ x: UnsafeMutableRawPointer) {}
func takesConstVoidPointer(_ x: UnsafeRawPointer) {}
func takesMutableRawPointer(_ x: UnsafeMutableRawPointer) {}
func takesConstRawPointer(_ x: UnsafeRawPointer) {}

// CHECK-LABEL: sil hidden @_TF18pointer_conversion16pointerToPointerFTGSpSi_GSPSi_Sv_T_
// CHECK: bb0([[MP:%.*]] : $UnsafeMutablePointer<Int>, [[CP:%.*]] : $UnsafePointer<Int>, [[MRP:%.*]] : $UnsafeMutableRawPointer):
func pointerToPointer(_ mp: UnsafeMutablePointer<Int>,
  _ cp: UnsafePointer<Int>, _ mrp: UnsafeMutableRawPointer) {

  // There should be no conversion here
  takesMutablePointer(mp)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointer
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[MP]])

  takesMutableVoidPointer(mp)
  // CHECK: [[TAKES_MUTABLE_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion23takesMutableVoidPointer
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE_VOID_POINTER]]

  takesMutableRawPointer(mp)
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion22takesMutableRawPointerFSvT_ :
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]

  takesConstPointer(mp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_TF18pointer_conversion17takesConstPointerFGSPSi_T_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafePointer<Int>>
  // CHECK: apply [[TAKES_CONST_POINTER]]

  takesConstVoidPointer(mp)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion21takesConstVoidPointerFSVT_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(mp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion20takesConstRawPointerFSVT_ :
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstPointer(cp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_TF18pointer_conversion17takesConstPointerFGSPSi_T_
  // CHECK: apply [[TAKES_CONST_POINTER]]([[CP]])

  takesConstVoidPointer(cp)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion21takesConstVoidPointerFSVT_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(cp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion20takesConstRawPointerFSVT_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstRawPointer(mrp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion20takesConstRawPointerFSVT_
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs32_convertPointerToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]
}

// CHECK-LABEL: sil hidden @_TF18pointer_conversion14arrayToPointerFT_T_
func arrayToPointer() {
  var ints = [1,2,3]

  takesMutablePointer(&ints)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointer
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_TFs37_convertMutableArrayToPointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutablePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstPointer(ints)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_TF18pointer_conversion17takesConstPointerFGSPSi_T_
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_TFs35_convertConstArrayToPointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesMutableRawPointer(&ints)
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion22takesMutableRawPointerFSvT_ :
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_TFs37_convertMutableArrayToPointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutableRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(ints)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion20takesConstRawPointerFSVT_ :
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_TFs35_convertConstArrayToPointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_TF18pointer_conversion15stringToPointerFSST_ 
func stringToPointer(_ s: String) {
  takesConstVoidPointer(s)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_TF18pointer_conversion21takesConstVoidPointerFSV
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_TFs40_convertConstStringToUTF8PointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(s)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_TF18pointer_conversion20takesConstRawPointerFSV
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_TFs40_convertConstStringToUTF8PointerArgument
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_TF18pointer_conversion14inoutToPointerFT_T_ 
func inoutToPointer() {
  var int = 0
  // CHECK: [[INT:%.*]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[INT]]
  takesMutablePointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointer
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_MUTABLE]]

  var logicalInt: Int {
    get { return 0 }
    set { }
  }
  takesMutablePointer(&logicalInt)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion19takesMutablePointer
  // CHECK: [[GETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_gL_10logicalIntSi
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_sL_10logicalIntSi
  // CHECK: apply [[SETTER]]

  takesMutableRawPointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion22takesMutableRawPointer
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_MUTABLE]]

  takesMutableRawPointer(&logicalInt)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_TF18pointer_conversion22takesMutableRawPointer
  // CHECK: [[GETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_gL_10logicalIntSi
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @_TFF18pointer_conversion14inoutToPointerFT_T_sL_10logicalIntSi
  // CHECK: apply [[SETTER]]
}

class C {}

func takesPlusOnePointer(_ x: UnsafeMutablePointer<C>) {}
func takesPlusZeroPointer(_ x: AutoreleasingUnsafeMutablePointer<C>) {}
func takesPlusZeroOptionalPointer(_ x: AutoreleasingUnsafeMutablePointer<C?>) {}

// CHECK-LABEL: sil hidden @_TF18pointer_conversion19classInoutToPointerFT_T_
func classInoutToPointer() {
  var c = C()
  // CHECK: [[VAR:%.*]] = alloc_box ${ var C }
  // CHECK: [[PB:%.*]] = project_box [[VAR]]
  takesPlusOnePointer(&c)
  // CHECK: [[TAKES_PLUS_ONE:%.*]] = function_ref @_TF18pointer_conversion19takesPlusOnePointer
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_PLUS_ONE]]

  takesPlusZeroPointer(&c)
  // CHECK: [[TAKES_PLUS_ZERO:%.*]] = function_ref @_TF18pointer_conversion20takesPlusZeroPointerFGVs33AutoreleasingUnsafeMutablePointerCS_1C_T_
  // CHECK: [[WRITEBACK:%.*]] = alloc_stack $@sil_unmanaged C
  // CHECK: [[OWNED:%.*]] = load_borrow [[PB]]
  // CHECK: [[UNOWNED:%.*]] = ref_to_unmanaged [[OWNED]]
  // CHECK: store [[UNOWNED]] to [trivial] [[WRITEBACK]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITEBACK]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_TFs30_convertInOutToPointerArgument
  // CHECK: apply [[CONVERT]]<AutoreleasingUnsafeMutablePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_PLUS_ZERO]]
  // CHECK: [[UNOWNED_OUT:%.*]] = load [trivial] [[WRITEBACK]]
  // CHECK: [[OWNED_OUT:%.*]] = unmanaged_to_ref [[UNOWNED_OUT]]
  // CHECK: [[OWNED_OUT_COPY:%.*]] = copy_value [[OWNED_OUT]]
  // CHECK: assign [[OWNED_OUT_COPY]] to [[PB]]

  var cq: C? = C()
  takesPlusZeroOptionalPointer(&cq)
}

// Check that pointer types don't bridge anymore.
@objc class ObjCMethodBridging : NSObject {
  // CHECK-LABEL: sil hidden [thunk] @_TToFC18pointer_conversion18ObjCMethodBridging11pointerArgs{{.*}} : $@convention(objc_method) (UnsafeMutablePointer<Int>, UnsafePointer<Int>, AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>, ObjCMethodBridging)
  @objc func pointerArgs(_ x: UnsafeMutablePointer<Int>,
                         y: UnsafePointer<Int>,
                         z: AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>) {}
}

// rdar://problem/21505805
// CHECK-LABEL: sil hidden @_TF18pointer_conversion22functionInoutToPointerFT_T_
func functionInoutToPointer() {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @callee_owned () -> () }
  var f: () -> () = {}

  // CHECK: [[REABSTRACT_BUF:%.*]] = alloc_stack $@callee_owned (@in ()) -> @out ()
  // CHECK: address_to_pointer [[REABSTRACT_BUF]]
  takesMutableVoidPointer(&f)
}
