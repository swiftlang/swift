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

// CHECK-LABEL: sil hidden @_T018pointer_conversion0A9ToPointerySpySiG_SPySiGSvtF
// CHECK: bb0([[MP:%.*]] : $UnsafeMutablePointer<Int>, [[CP:%.*]] : $UnsafePointer<Int>, [[MRP:%.*]] : $UnsafeMutableRawPointer):
func pointerToPointer(_ mp: UnsafeMutablePointer<Int>,
  _ cp: UnsafePointer<Int>, _ mrp: UnsafeMutableRawPointer) {

  // There should be no conversion here
  takesMutablePointer(mp)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[MP]])

  takesMutableVoidPointer(mp)
  // CHECK: [[TAKES_MUTABLE_VOID_POINTER:%.*]] = function_ref @_T018pointer_conversion23takesMutableVoidPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE_VOID_POINTER]]

  takesMutableRawPointer(mp)
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion22takesMutableRawPointerySvF :
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]

  takesConstPointer(mp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_T018pointer_conversion17takesConstPointerySPySiGF
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafePointer<Int>>
  // CHECK: apply [[TAKES_CONST_POINTER]]

  takesConstVoidPointer(mp)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_T018pointer_conversion21takesConstVoidPointerySVF
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(mp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySVF :
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstPointer(cp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_T018pointer_conversion17takesConstPointerySPySiGF
  // CHECK: apply [[TAKES_CONST_POINTER]]([[CP]])

  takesConstVoidPointer(cp)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_T018pointer_conversion21takesConstVoidPointerySVF
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(cp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySVF
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstRawPointer(mrp)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySVF
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer, UnsafeRawPointer>
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion14arrayToPointeryyF
func arrayToPointer() {
  var ints = [1,2,3]

  takesMutablePointer(&ints)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_T0s37_convertMutableArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutablePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstPointer(ints)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_T018pointer_conversion17takesConstPointerySPySiGF
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesMutableRawPointer(&ints)
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion22takesMutableRawPointerySvF :
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_T0s37_convertMutableArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutableRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(ints)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySVF :
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion15stringToPointerySSF 
func stringToPointer(_ s: String) {
  takesConstVoidPointer(s)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_T018pointer_conversion21takesConstVoidPointerySV{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(s)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySV{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[POINTER]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion14inoutToPointeryyF 
func inoutToPointer() {
  var int = 0
  // CHECK: [[INT:%.*]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[INT]]
  takesMutablePointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_MUTABLE]]

  var logicalInt: Int {
    get { return 0 }
    set { }
  }
  takesMutablePointer(&logicalInt)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[GETTER:%.*]] = function_ref @_T018pointer_conversion14inoutToPointeryyF10logicalIntL_Sifg
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @_T018pointer_conversion14inoutToPointeryyF10logicalIntL_Sifs
  // CHECK: apply [[SETTER]]

  takesMutableRawPointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion22takesMutableRawPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_MUTABLE]]

  takesMutableRawPointer(&logicalInt)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion22takesMutableRawPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[GETTER:%.*]] = function_ref @_T018pointer_conversion14inoutToPointeryyF10logicalIntL_Sifg
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @_T018pointer_conversion14inoutToPointeryyF10logicalIntL_Sifs
  // CHECK: apply [[SETTER]]
}

class C {}

func takesPlusOnePointer(_ x: UnsafeMutablePointer<C>) {}
func takesPlusZeroPointer(_ x: AutoreleasingUnsafeMutablePointer<C>) {}
func takesPlusZeroOptionalPointer(_ x: AutoreleasingUnsafeMutablePointer<C?>) {}

// CHECK-LABEL: sil hidden @_T018pointer_conversion19classInoutToPointeryyF
func classInoutToPointer() {
  var c = C()
  // CHECK: [[VAR:%.*]] = alloc_box ${ var C }
  // CHECK: [[PB:%.*]] = project_box [[VAR]]
  takesPlusOnePointer(&c)
  // CHECK: [[TAKES_PLUS_ONE:%.*]] = function_ref @_T018pointer_conversion19takesPlusOnePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[PB]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: apply [[TAKES_PLUS_ONE]]

  takesPlusZeroPointer(&c)
  // CHECK: [[TAKES_PLUS_ZERO:%.*]] = function_ref @_T018pointer_conversion20takesPlusZeroPointerys026AutoreleasingUnsafeMutableF0VyAA1CCGF
  // CHECK: [[WRITEBACK:%.*]] = alloc_stack $@sil_unmanaged C
  // CHECK: [[OWNED:%.*]] = load_borrow [[PB]]
  // CHECK: [[UNOWNED:%.*]] = ref_to_unmanaged [[OWNED]]
  // CHECK: store [[UNOWNED]] to [trivial] [[WRITEBACK]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITEBACK]]
  // CHECK: [[CONVERT:%.*]] = function_ref @_T0s30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
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
  // CHECK-LABEL: sil hidden [thunk] @_T018pointer_conversion18ObjCMethodBridgingC0A4Args{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (UnsafeMutablePointer<Int>, UnsafePointer<Int>, AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>, ObjCMethodBridging)
  @objc func pointerArgs(_ x: UnsafeMutablePointer<Int>,
                         y: UnsafePointer<Int>,
                         z: AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>) {}
}

// rdar://problem/21505805
// CHECK-LABEL: sil hidden @_T018pointer_conversion22functionInoutToPointeryyF
func functionInoutToPointer() {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @callee_owned () -> () }
  var f: () -> () = {}

  // CHECK: [[REABSTRACT_BUF:%.*]] = alloc_stack $@callee_owned (@in ()) -> @out ()
  // CHECK: address_to_pointer [[REABSTRACT_BUF]]
  takesMutableVoidPointer(&f)
}
