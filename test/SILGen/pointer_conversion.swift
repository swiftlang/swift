// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name pointer_conversion -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// REQUIRES: objc_interop

import Foundation

func sideEffect1() -> Int { return 1 }
func sideEffect2() -> Int { return 2 }
func takesMutablePointer(_ x: UnsafeMutablePointer<Int>) {}
func takesConstPointer(_ x: UnsafePointer<Int>) {}
func takesOptConstPointer(_ x: UnsafePointer<Int>?, and: Int) {}
func takesOptOptConstPointer(_ x: UnsafePointer<Int>??, and: Int) {}
func takesMutablePointer(_ x: UnsafeMutablePointer<Int>, and: Int) {}
func takesConstPointer(_ x: UnsafePointer<Int>, and: Int) {}
func takesMutableVoidPointer(_ x: UnsafeMutableRawPointer) {}
func takesConstVoidPointer(_ x: UnsafeRawPointer) {}
func takesMutableRawPointer(_ x: UnsafeMutableRawPointer) {}
func takesConstRawPointer(_ x: UnsafeRawPointer) {}
func takesOptConstRawPointer(_ x: UnsafeRawPointer?, and: Int) {}
func takesOptOptConstRawPointer(_ x: UnsafeRawPointer??, and: Int) {}
func takesMutableFunctionPointer(_ x: UnsafeMutablePointer<() -> Void>) {}

@_silgen_name("takeObjectPointer")
func takeObjectPointer(_: UnsafePointer<AnyObject>)

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion0A9ToPointeryySpySiG_SPySiGSvtF
// CHECK: bb0([[MP:%.*]] : $UnsafeMutablePointer<Int>, [[CP:%.*]] : $UnsafePointer<Int>, [[MRP:%.*]] : $UnsafeMutableRawPointer):
func pointerToPointer(_ mp: UnsafeMutablePointer<Int>,
  _ cp: UnsafePointer<Int>, _ mrp: UnsafeMutableRawPointer) {

  // There should be no conversion here
  takesMutablePointer(mp)
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @$s18pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[MP]])

  takesMutableVoidPointer(mp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: [[TAKES_MUTABLE_VOID_POINTER:%.*]] = function_ref @$s18pointer_conversion23takesMutableVoidPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE_VOID_POINTER]]

  takesMutableRawPointer(mp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeMutableRawPointer>
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion22takesMutableRawPointeryySvF :
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]

  takesConstPointer(mp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafePointer<Int>>
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @$s18pointer_conversion17takesConstPointeryySPySiGF
  // CHECK: apply [[TAKES_CONST_POINTER]]

  takesConstVoidPointer(mp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @$s18pointer_conversion21takesConstVoidPointeryySVF
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(mp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>, UnsafeRawPointer>
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesConstRawPointeryySVF :
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstPointer(cp)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @$s18pointer_conversion17takesConstPointeryySPySiGF
  // CHECK: apply [[TAKES_CONST_POINTER]]([[CP]])

  takesConstVoidPointer(cp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @$s18pointer_conversion21takesConstVoidPointeryySVF
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]

  takesConstRawPointer(cp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafePointer<Int>, UnsafeRawPointer>
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesConstRawPointeryySVF
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]

  takesConstRawPointer(mrp)
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss017_convertPointerToB8Argument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer, UnsafeRawPointer>
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesConstRawPointeryySVF
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion14arrayToPointeryyF
func arrayToPointer() {
  var ints = [1,2,3]

  takesMutablePointer(&ints)
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @$ss37_convertMutableArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutablePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeMutablePointer<Int> on [[OWNER]]
  // CHECK: [[TAKES_MUTABLE_POINTER:%.*]] = function_ref @$s18pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstPointer(ints)
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @$ss35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @$s18pointer_conversion17takesConstPointeryySPySiGF
  // CHECK: apply [[TAKES_CONST_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesMutableRawPointer(&ints)
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @$ss37_convertMutableArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutableRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on [[OWNER]]
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion22takesMutableRawPointeryySvF :
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(ints)
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @$ss35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesConstRawPointeryySVF :
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesOptConstPointer(ints, and: sideEffect1())
  // CHECK: [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @$ss35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK: [[OPTPTR:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.some!enumelt, [[DEPENDENT]]
  // CHECK: [[TAKES_OPT_CONST_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesOptConstPointer_3andySPySiGSg_SitF :
  // CHECK: apply [[TAKES_OPT_CONST_POINTER]]([[OPTPTR]], [[RESULT1]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion15stringToPointeryySSF
func stringToPointer(_ s: String) {
  takesConstVoidPointer(s)
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @$ss40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @$s18pointer_conversion21takesConstVoidPointeryySV{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(s)
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @$ss40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion20takesConstRawPointeryySV{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesOptConstRawPointer(s, and: sideEffect1())
  // CHECK: [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @$ss40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: [[OPTPTR:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt, [[DEPENDENT]]
  // CHECK: [[TAKES_OPT_CONST_RAW_POINTER:%.*]] = function_ref @$s18pointer_conversion23takesOptConstRawPointer_3andySVSg_SitF :
  // CHECK: apply [[TAKES_OPT_CONST_RAW_POINTER]]([[OPTPTR]], [[RESULT1]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion14inoutToPointeryyF 
func inoutToPointer() {
  var int = 0
  // CHECK: [[INT:%.*]] = alloc_box ${ var Int }
  // CHECK: [[L:%.*]] = begin_borrow [var_decl] [[INT]]
  // CHECK: [[PB:%.*]] = project_box [[L]]
  takesMutablePointer(&int)
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [stack_protection] [[WRITE]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>({{%.*}}, [[POINTER]])
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @$s18pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE]]

  var logicalInt: Int {
    get { return 0 }
    set { }
  }
  takesMutablePointer(&logicalInt)
  // CHECK: [[GETTER:%.*]] = function_ref @$s18pointer_conversion14inoutToPointeryyF10logicalIntL_Sivg
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Int>>
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @$s18pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @$s18pointer_conversion14inoutToPointeryyF10logicalIntL_Sivs
  // CHECK: apply [[SETTER]]

  takesMutableRawPointer(&int)
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [stack_protection] [[WRITE]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>({{%.*}}, [[POINTER]])
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @$s18pointer_conversion22takesMutableRawPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE]]

  takesMutableRawPointer(&logicalInt)
  // CHECK: [[GETTER:%.*]] = function_ref @$s18pointer_conversion14inoutToPointeryyF10logicalIntL_Sivg
  // CHECK: apply [[GETTER]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutableRawPointer>
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @$s18pointer_conversion22takesMutableRawPointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_MUTABLE]]
  // CHECK: [[SETTER:%.*]] = function_ref @$s18pointer_conversion14inoutToPointeryyF10logicalIntL_Sivs
  // CHECK: apply [[SETTER]]
}

class C {}

func takesPlusOnePointer(_ x: UnsafeMutablePointer<C>) {}
func takesPlusZeroPointer(_ x: AutoreleasingUnsafeMutablePointer<C>) {}
func takesPlusZeroOptionalPointer(_ x: AutoreleasingUnsafeMutablePointer<C?>) {}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion19classInoutToPointeryyF
func classInoutToPointer() {
  var c = C()
  // CHECK: [[VAR:%.*]] = alloc_box ${ var C }
  // CHECK: [[LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[VAR]]
  // CHECK: [[PB:%.*]] = project_box [[LIFETIME]]
  takesPlusOnePointer(&c)
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [stack_protection] [[WRITE]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<UnsafeMutablePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: [[TAKES_PLUS_ONE:%.*]] = function_ref @$s18pointer_conversion19takesPlusOnePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[TAKES_PLUS_ONE]]

  takesPlusZeroPointer(&c)
  // CHECK: [[WRITE2:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[WRITEBACK:%.*]] = alloc_stack $@sil_unmanaged C
  // CHECK: [[OWNED:%.*]] = load_borrow [[WRITE2]]
  // CHECK: [[UNOWNED:%.*]] = ref_to_unmanaged [[OWNED]]
  // CHECK: store [[UNOWNED]] to [trivial] [[WRITEBACK]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [stack_protection] [[WRITEBACK]]
  // CHECK: [[CONVERT:%.*]] = function_ref @$ss30_convertInOutToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[CONVERT]]<AutoreleasingUnsafeMutablePointer<C>>({{%.*}}, [[POINTER]])
  // CHECK: [[TAKES_PLUS_ZERO:%.*]] = function_ref @$s18pointer_conversion20takesPlusZeroPointeryySAyAA1CCGF
  // CHECK: apply [[TAKES_PLUS_ZERO]]
  // CHECK: [[UNOWNED_OUT:%.*]] = load [trivial] [[WRITEBACK]]
  // CHECK: [[OWNED_OUT:%.*]] = unmanaged_to_ref [[UNOWNED_OUT]]
  // CHECK: [[OWNED_OUT_COPY:%.*]] = copy_value [[OWNED_OUT]]
  //
  // DISCUSSION: We need a mark_dependence here to ensure that the destroy of
  // the value in WRITE2 is not hoisted above the copy of OWNED_OUT. Otherwise,
  // we may have a use-after-free if ref counts are low enough.
  //
  // CHECK: [[OWNED_OUT_COPY_DEP:%.*]] = mark_dependence [[OWNED_OUT_COPY]] : $C on [[WRITE2]]
  // CHECK: assign [[OWNED_OUT_COPY_DEP]] to [[WRITE2]]

  var cq: C? = C()
  takesPlusZeroOptionalPointer(&cq)
}

// Check that pointer types don't bridge anymore.
@objc class ObjCMethodBridging : NSObject {
  // CHECK-LABEL: sil private [thunk] [ossa] @$s18pointer_conversion18ObjCMethodBridgingC0A4Args{{[_0-9a-zA-Z]*}}FTo : $@convention(objc_method) (UnsafeMutablePointer<Int>, UnsafePointer<Int>, AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>, ObjCMethodBridging)
  @objc func pointerArgs(_ x: UnsafeMutablePointer<Int>,
                         y: UnsafePointer<Int>,
                         z: AutoreleasingUnsafeMutablePointer<ObjCMethodBridging>) {}
}

// rdar://problem/21505805
// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion22functionInoutToPointeryyF
func functionInoutToPointer() {
  // CHECK: [[BOX:%.*]] = alloc_box ${ var @callee_guaranteed () -> () }
  var f: () -> () = {}

  // CHECK: [[REABSTRACT_BUF:%.*]] = alloc_stack $@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>
  // CHECK: address_to_pointer [stack_protection] [[REABSTRACT_BUF]]
  takesMutableFunctionPointer(&f)
}

// rdar://problem/31781386
// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion20inoutPointerOrderingyyF
func inoutPointerOrdering() {
  // CHECK: [[ARRAY_BOX:%.*]] = alloc_box ${ var Array<Int> }
  // CHECK: [[L:%.*]] = begin_borrow [var_decl] [[ARRAY_BOX]] :
  // CHECK: [[ARRAY:%.*]] = project_box [[L]] :
  // CHECK: store {{.*}} to [init] [[ARRAY]]
  var array = [Int]()

  // CHECK: [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[SIDE2:%.*]] = function_ref @$s18pointer_conversion11sideEffect2SiyF
  // CHECK: [[RESULT2:%.*]] = apply [[SIDE2]]()
  // CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[ARRAY]] : $*Array<Int>
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @$s18pointer_conversion19takesMutablePointer_3andySpySiG_SitF
  // CHECK: apply [[TAKES_MUTABLE]]({{.*}}, [[RESULT2]])
  // CHECK: end_access [[ACCESS]]
  takesMutablePointer(&array[sideEffect1()], and: sideEffect2())

  // CHECK: [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[SIDE2:%.*]] = function_ref @$s18pointer_conversion11sideEffect2SiyF
  // CHECK: [[RESULT2:%.*]] = apply [[SIDE2]]()
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[ARRAY]] : $*Array<Int>
  // CHECK: [[TAKES_CONST:%.*]] = function_ref @$s18pointer_conversion17takesConstPointer_3andySPySiG_SitF
  // CHECK: apply [[TAKES_CONST]]({{.*}}, [[RESULT2]])
  // CHECK: end_access [[ACCESS]]
  takesConstPointer(&array[sideEffect1()], and: sideEffect2())
}

// rdar://problem/31542269
// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion20optArrayToOptPointer5arrayySaySiGSg_tF
func optArrayToOptPointer(array: [Int]?) {
  // CHECK:   [[COPY:%.*]] = copy_value %0
  // CHECK:   [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   switch_enum [[COPY]] : $Optional<Array<Int>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[SOME_VALUE:%.*]] :
  // CHECK:   [[CONVERT:%.*]] = function_ref @$ss35_convertConstArrayToPointerArgumentyyXlSg_q_tSayxGs01_E0R_r0_lF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafePointer<Int>
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<Int, UnsafePointer<Int>>([[TEMP:%.*]], [[SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.some!enumelt, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafePointer<Int>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafePointer<Int>>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafePointer<Int>> on [[OWNER]]
  // CHECK:   [[TAKES:%.*]] = function_ref @$s18pointer_conversion20takesOptConstPointer_3andySPySiGSg_SitF
  // CHECK:   apply [[TAKES]]([[OPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK-NOT:   destroy_value %0
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[CONT_BB]]([[NO_VALUE]] : $Optional<UnsafePointer<Int>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptConstPointer(array, and: sideEffect1())
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion013optOptArrayTodD7Pointer5arrayySaySiGSgSg_tF
func optOptArrayToOptOptPointer(array: [Int]??) {
  // CHECK:   [[COPY:%.*]] = copy_value %0
  // CHECK:   [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   switch_enum [[COPY]] : $Optional<Optional<Array<Int>>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[SOME_VALUE:%.*]] :
  // CHECK:   switch_enum [[SOME_VALUE]] : $Optional<Array<Int>>, case #Optional.some!enumelt: [[SOME_SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[SOME_NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_SOME_BB]]([[SOME_SOME_VALUE:%.*]] :
  // CHECK:   [[CONVERT:%.*]] = function_ref @$ss35_convertConstArrayToPointerArgumentyyXlSg_q_tSayxGs01_E0R_r0_lF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafePointer<Int>
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<Int, UnsafePointer<Int>>([[TEMP:%.*]], [[SOME_SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.some!enumelt, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[SOME_SOME_CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafePointer<Int>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_SOME_CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafePointer<Int>>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafePointer<Int>> on [[OWNER]]
  // CHECK:   [[OPTOPTPTR:%.*]] = enum $Optional<Optional<UnsafePointer<Int>>>, #Optional.some!enumelt, [[OPTDEP]]
  // CHECK:   br [[SOME_CONT_BB:bb[0-9]+]]([[OPTOPTPTR]] : $Optional<Optional<UnsafePointer<Int>>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_CONT_BB]]([[OPTOPTPTR:%.*]] : $Optional<Optional<UnsafePointer<Int>>>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTOPTDEP:%.*]] = mark_dependence [[OPTOPTPTR]] : $Optional<Optional<UnsafePointer<Int>>> on [[OWNER]]
  // CHECK:   [[TAKES:%.*]] = function_ref @$s18pointer_conversion08takesOptD12ConstPointer_3andySPySiGSgSg_SitF
  // CHECK:   apply [[TAKES]]([[OPTOPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK-NOT:   destroy_value %0
  // CHECK: [[SOME_NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_SOME_CONT_BB]]([[NO_VALUE]] : $Optional<UnsafePointer<Int>>, [[NO_OWNER]] : $Optional<AnyObject>)
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<Optional<UnsafePointer<Int>>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_CONT_BB]]([[NO_VALUE]] : $Optional<Optional<UnsafePointer<Int>>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptOptConstPointer(array, and: sideEffect1())
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion21optStringToOptPointer6stringySSSg_tF
func optStringToOptPointer(string: String?) {
  // CHECK:   [[COPY:%.*]] = copy_value %0
  // CHECK:   [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   switch_enum [[COPY]] : $Optional<String>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[SOME_VALUE:%.*]] :
  // CHECK:   [[CONVERT:%.*]] = function_ref @$ss40_convertConstStringToUTF8PointerArgumentyyXlSg_xtSSs01_F0RzlF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafeRawPointer
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<UnsafeRawPointer>([[TEMP:%.*]], [[SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafeRawPointer>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafeRawPointer>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafeRawPointer> on [[OWNER]]
  // CHECK:   [[TAKES:%.*]] = function_ref @$s18pointer_conversion23takesOptConstRawPointer_3andySVSg_SitF
  // CHECK:   apply [[TAKES]]([[OPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK-NOT:   destroy_value %0
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[CONT_BB]]([[NO_VALUE]] : $Optional<UnsafeRawPointer>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptConstRawPointer(string, and: sideEffect1())
}

// CHECK-LABEL: sil hidden [ossa] @$s18pointer_conversion014optOptStringTodD7Pointer6stringySSSgSg_tF
func optOptStringToOptOptPointer(string: String??) {
  // CHECK:   [[COPY:%.*]] = copy_value %0
  // CHECK:   [[SIDE1:%.*]] = function_ref @$s18pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  //   FIXME: this should really go somewhere that will make nil, not some(nil)
  // CHECK:   switch_enum [[COPY]] : $Optional<Optional<String>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[SOME_VALUE:%.*]] :
  // CHECK:   switch_enum [[SOME_VALUE]] : $Optional<String>, case #Optional.some!enumelt: [[SOME_SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[SOME_NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_SOME_BB]]([[SOME_SOME_VALUE:%.*]] :
  // CHECK:   [[CONVERT:%.*]] = function_ref @$ss40_convertConstStringToUTF8PointerArgumentyyXlSg_xtSSs01_F0RzlF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafeRawPointer
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<UnsafeRawPointer>([[TEMP:%.*]], [[SOME_SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[SOME_SOME_CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafeRawPointer>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_SOME_CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafeRawPointer>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafeRawPointer> on [[OWNER]]
  // CHECK:   [[OPTOPTPTR:%.*]] = enum $Optional<Optional<UnsafeRawPointer>>, #Optional.some!enumelt, [[OPTDEP]]
  // CHECK:   br [[SOME_CONT_BB:bb[0-9]+]]([[OPTOPTPTR]] : $Optional<Optional<UnsafeRawPointer>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_CONT_BB]]([[OPTOPTPTR:%.*]] : $Optional<Optional<UnsafeRawPointer>>, [[OWNER:%.*]] : @owned $Optional<AnyObject>):
  // CHECK:   [[OPTOPTDEP:%.*]] = mark_dependence [[OPTOPTPTR]] : $Optional<Optional<UnsafeRawPointer>> on [[OWNER]]
  // CHECK:   [[TAKES:%.*]] = function_ref @$s18pointer_conversion08takesOptD15ConstRawPointer_3andySVSgSg_SitF
  // CHECK:   apply [[TAKES]]([[OPTOPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK-NOT:   destroy_value %0
  // CHECK: [[SOME_NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_SOME_CONT_BB]]([[NO_VALUE]] : $Optional<UnsafeRawPointer>, [[NO_OWNER]] : $Optional<AnyObject>)
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<Optional<UnsafeRawPointer>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_CONT_BB]]([[NO_VALUE]] : $Optional<Optional<UnsafeRawPointer>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptOptConstRawPointer(string, and: sideEffect1())
}

final public class Obj {
  var object: AnyObject

  init(object: AnyObject) { self.object = object }
}

public struct RefObj {
  var o: Obj
}

// CHECK-LABEL: sil [ossa] @$s18pointer_conversion20objectFieldToPointer2rcyAA6RefObjV_tF : $@convention(thin) (@guaranteed RefObj) -> () {
// CHECK: bb0(%0 : @guaranteed $RefObj):
// CHECK:   [[B:%.*]] = begin_borrow %{{.*}} : $Obj
// CHECK:   [[R:%.*]] = ref_element_addr [[B]] : $Obj, #Obj.object
// CHECK:   [[A:%.*]] = begin_access [read] [dynamic] [[R]] : $*AnyObject
// CHECK:   [[P:%.*]] = address_to_pointer [stack_protection] [[A]] : $*AnyObject to $Builtin.RawPointer
// CHECK:   apply %{{.*}}<UnsafePointer<AnyObject>>(%{{.*}}, %7) : $@convention(thin) <τ_0_0 where τ_0_0 : _Pointer> (Builtin.RawPointer) -> @out τ_0_0
// CHECK:   apply {{.*}} : $@convention(thin) (UnsafePointer<AnyObject>) -> ()
// CHECK:   fix_lifetime [[A]] : $*AnyObject
// CHECK:   end_access [[A]] : $*AnyObject
// CHECK:   end_borrow [[B]] : $Obj
// CHECK:   destroy_value %{{.*}} : $Obj
// CHECK:   dealloc_stack %{{.*}} : $*UnsafePointer<AnyObject>
// CHECK-LABEL: } // end sil function '$s18pointer_conversion20objectFieldToPointer2rcyAA6RefObjV_tF'
public func objectFieldToPointer(rc: RefObj) {
  takeObjectPointer(&rc.o.object)
}

// CHECK-LABEL: sil [ossa] @$s18pointer_conversion21testVariadicParameter1aySaySiGz_tF : $@convention(thin) (@inout Array<Int>) -> ()
// CHECK: [[ARRAY_REF:%.*]] = begin_access [modify] [unknown] %0 : $*Array<Int>
// CHECK: [[CONVERT_ARRAY_TO_POINTER:%.*]] = function_ref @$ss37_convertMutableArrayToPointerArgumentyyXlSg_q_tSayxGzs01_E0R_r0_lF
// CHECK: apply [[CONVERT_ARRAY_TO_POINTER]]<Int, UnsafeMutableRawPointer>({{.*}}, [[ARRAY_REF]])
// CHECK: [[V_REF:%.*]] = begin_access [modify] [unknown] %28 : $*Double
// CHECK: [[DOUBLE_AS_PTR:%.*]] = address_to_pointer [stack_protection] [[V_REF]] : $*Double to $Builtin.RawPointer
// CHECK: [[INOUT_TO_PTR:%.*]] = function_ref @$ss30_convertInOutToPointerArgumentyxBps01_E0RzlF
// CHECK: apply [[INOUT_TO_PTR]]<UnsafeMutableRawPointer>(%39, [[DOUBLE_AS_PTR]])
// CHECK: } // end sil function '$s18pointer_conversion21testVariadicParameter1aySaySiGz_tF'
public func testVariadicParameter(a: inout [Int]) {
  func test(_ : UnsafeMutableRawPointer?...) {}

  test(&a)

  var v: Double
  test(&v)
}
