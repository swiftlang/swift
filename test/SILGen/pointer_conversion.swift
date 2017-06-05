// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

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
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeMutablePointer<Int> on [[OWNER]]
  // CHECK: apply [[TAKES_MUTABLE_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstPointer(ints)
  // CHECK: [[TAKES_CONST_POINTER:%.*]] = function_ref @_T018pointer_conversion17takesConstPointerySPySiGF
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafePointer<Int>>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK: apply [[TAKES_CONST_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesMutableRawPointer(&ints)
  // CHECK: [[TAKES_MUTABLE_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion22takesMutableRawPointerySvF :
  // CHECK: [[CONVERT_MUTABLE:%.*]] = function_ref @_T0s37_convertMutableArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_MUTABLE]]<Int, UnsafeMutableRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeMutableRawPointer on [[OWNER]]
  // CHECK: apply [[TAKES_MUTABLE_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(ints)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySVF :
  // CHECK: [[CONVERT_CONST:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_CONST]]<Int, UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion15stringToPointerySSF 
func stringToPointer(_ s: String) {
  takesConstVoidPointer(s)
  // CHECK: [[TAKES_CONST_VOID_POINTER:%.*]] = function_ref @_T018pointer_conversion21takesConstVoidPointerySV{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: apply [[TAKES_CONST_VOID_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]

  takesConstRawPointer(s)
  // CHECK: [[TAKES_CONST_RAW_POINTER:%.*]] = function_ref @_T018pointer_conversion20takesConstRawPointerySV{{[_0-9a-zA-Z]*}}F
  // CHECK: [[CONVERT_STRING:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgument{{[_0-9a-zA-Z]*}}F
  // CHECK: [[OWNER:%.*]] = apply [[CONVERT_STRING]]<UnsafeRawPointer>([[POINTER_BUF:%[0-9]*]],
  // CHECK: [[POINTER:%.*]] = load [trivial] [[POINTER_BUF]]
  // CHECK: [[DEPENDENT:%.*]] = mark_dependence [[POINTER]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK: apply [[TAKES_CONST_RAW_POINTER]]([[DEPENDENT]])
  // CHECK: destroy_value [[OWNER]]
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion14inoutToPointeryyF 
func inoutToPointer() {
  var int = 0
  // CHECK: [[INT:%.*]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[INT]]
  takesMutablePointer(&int)
  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointer{{[_0-9a-zA-Z]*}}F
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITE]]
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
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITE]]
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
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[PB]]
  // CHECK: [[POINTER:%.*]] = address_to_pointer [[WRITE]]
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

// rdar://problem/31781386
// CHECK-LABEL: sil hidden @_T018pointer_conversion20inoutPointerOrderingyyF
func inoutPointerOrdering() {
  // CHECK: [[ARRAY_BOX:%.*]] = alloc_box ${ var Array<Int> }
  // CHECK: [[ARRAY:%.*]] = project_box [[ARRAY_BOX]] :
  // CHECK: store {{.*}} to [init] [[ARRAY]]
  var array = [Int]()

  // CHECK: [[TAKES_MUTABLE:%.*]] = function_ref @_T018pointer_conversion19takesMutablePointerySpySiG_Si3andtF
  // CHECK: [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[SIDE2:%.*]] = function_ref @_T018pointer_conversion11sideEffect2SiyF
  // CHECK: [[RESULT2:%.*]] = apply [[SIDE2]]()
  // CHECK: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[ARRAY]] : $*Array<Int>
  // CHECK: apply [[TAKES_MUTABLE]]({{.*}}, [[RESULT2]])
  // CHECK: strong_unpin
  // CHECK: end_access [[ACCESS]]
  takesMutablePointer(&array[sideEffect1()], and: sideEffect2())

  // CHECK: [[TAKES_CONST:%.*]] = function_ref @_T018pointer_conversion17takesConstPointerySPySiG_Si3andtF
  // CHECK: [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK: [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK: [[SIDE2:%.*]] = function_ref @_T018pointer_conversion11sideEffect2SiyF
  // CHECK: [[RESULT2:%.*]] = apply [[SIDE2]]()
  // CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[ARRAY]] : $*Array<Int>
  // CHECK: apply [[TAKES_CONST]]({{.*}}, [[RESULT2]])
  // CHECK: end_access [[ACCESS]]
  takesConstPointer(&array[sideEffect1()], and: sideEffect2())
}

// rdar://problem/31542269
// CHECK-LABEL: sil hidden @_T018pointer_conversion20optArrayToOptPointerySaySiGSg5array_tF
func optArrayToOptPointer(array: [Int]?) {
  // CHECK:   [[TAKES:%.*]] = function_ref @_T018pointer_conversion20takesOptConstPointerySPySiGSg_Si3andtF
  // CHECK:   [[BORROW:%.*]] = begin_borrow %0
  // CHECK:   [[COPY:%.*]] = copy_value [[BORROW]]
  // CHECK:   [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   [[T0:%.*]] = select_enum [[COPY]]
  // CHECK:   cond_br [[T0]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]:
  // CHECK:   [[SOME_VALUE:%.*]] = unchecked_enum_data [[COPY]]
  // CHECK:   [[CONVERT:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgumentyXlSg_q_tSayxGs01_E0R_r0_lF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafePointer<Int>
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<Int, UnsafePointer<Int>>([[TEMP:%.*]], [[SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.some!enumelt.1, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafePointer<Int>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafePointer<Int>>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafePointer<Int>> on [[OWNER]]
  // CHECK:   apply [[TAKES]]([[OPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK:   end_borrow [[BORROW]]
  // CHECK:   destroy_value %0
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[CONT_BB]]([[NO_VALUE]] : $Optional<UnsafePointer<Int>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptConstPointer(array, and: sideEffect1())
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion013optOptArrayTodD7PointerySaySiGSgSg5array_tF
func optOptArrayToOptOptPointer(array: [Int]??) {
  // CHECK:   [[TAKES:%.*]] = function_ref @_T018pointer_conversion08takesOptD12ConstPointerySPySiGSgSg_Si3andtF
  // CHECK:   [[BORROW:%.*]] = begin_borrow %0
  // CHECK:   [[COPY:%.*]] = copy_value [[BORROW]]
  // CHECK:   [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   [[T0:%.*]] = select_enum [[COPY]]
  // CHECK:   cond_br [[T0]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]:
  // CHECK:   [[SOME_VALUE:%.*]] = unchecked_enum_data [[COPY]]
  // CHECK:   [[T0:%.*]] = select_enum [[SOME_VALUE]]
  // CHECK:   cond_br [[T0]], [[SOME_SOME_BB:bb[0-9]+]], [[SOME_NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_NONE_BB]]:
  // CHECK:   destroy_value [[SOME_VALUE]]
  // CHECK:   br [[SOME_NONE_BB2:bb[0-9]+]]
  // CHECK: [[SOME_SOME_BB]]:
  // CHECK:   [[SOME_SOME_VALUE:%.*]] = unchecked_enum_data [[SOME_VALUE]]
  // CHECK:   [[CONVERT:%.*]] = function_ref @_T0s35_convertConstArrayToPointerArgumentyXlSg_q_tSayxGs01_E0R_r0_lF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafePointer<Int>
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<Int, UnsafePointer<Int>>([[TEMP:%.*]], [[SOME_SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafePointer<Int> on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.some!enumelt.1, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[SOME_SOME_CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafePointer<Int>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_SOME_CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafePointer<Int>>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafePointer<Int>> on [[OWNER]]
  // CHECK:   [[OPTOPTPTR:%.*]] = enum $Optional<Optional<UnsafePointer<Int>>>, #Optional.some!enumelt.1, [[OPTDEP]]
  // CHECK:   br [[SOME_CONT_BB:bb[0-9]+]]([[OPTOPTPTR]] : $Optional<Optional<UnsafePointer<Int>>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_CONT_BB]]([[OPTOPTPTR:%.*]] : $Optional<Optional<UnsafePointer<Int>>>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTOPTDEP:%.*]] = mark_dependence [[OPTOPTPTR]] : $Optional<Optional<UnsafePointer<Int>>> on [[OWNER]]
  // CHECK:   apply [[TAKES]]([[OPTOPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK:   end_borrow [[BORROW]]
  // CHECK:   destroy_value %0
  // CHECK: [[SOME_NONE_BB2]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafePointer<Int>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_SOME_CONT_BB]]([[NO_VALUE]] : $Optional<UnsafePointer<Int>>, [[NO_OWNER]] : $Optional<AnyObject>)
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<Optional<UnsafePointer<Int>>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_CONT_BB]]([[NO_VALUE]] : $Optional<Optional<UnsafePointer<Int>>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptOptConstPointer(array, and: sideEffect1())
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion21optStringToOptPointerySSSg6string_tF
func optStringToOptPointer(string: String?) {
  // CHECK:   [[TAKES:%.*]] = function_ref @_T018pointer_conversion23takesOptConstRawPointerySVSg_Si3andtF
  // CHECK:   [[BORROW:%.*]] = begin_borrow %0
  // CHECK:   [[COPY:%.*]] = copy_value [[BORROW]]
  // CHECK:   [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   [[T0:%.*]] = select_enum [[COPY]]
  // CHECK:   cond_br [[T0]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]:
  // CHECK:   [[SOME_VALUE:%.*]] = unchecked_enum_data [[COPY]]
  // CHECK:   [[CONVERT:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgumentyXlSg_xtSSs01_F0RzlF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafeRawPointer
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<UnsafeRawPointer>([[TEMP:%.*]], [[SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt.1, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafeRawPointer>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafeRawPointer>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafeRawPointer> on [[OWNER]]
  // CHECK:   apply [[TAKES]]([[OPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK:   end_borrow [[BORROW]]
  // CHECK:   destroy_value %0
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[CONT_BB]]([[NO_VALUE]] : $Optional<UnsafeRawPointer>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptConstRawPointer(string, and: sideEffect1())
}

// CHECK-LABEL: sil hidden @_T018pointer_conversion014optOptStringTodD7PointerySSSgSg6string_tF
func optOptStringToOptOptPointer(string: String??) {
  // CHECK:   [[TAKES:%.*]] = function_ref @_T018pointer_conversion08takesOptD15ConstRawPointerySVSgSg_Si3andtF
  // CHECK:   [[BORROW:%.*]] = begin_borrow %0
  // CHECK:   [[COPY:%.*]] = copy_value [[BORROW]]
  // CHECK:   [[SIDE1:%.*]] = function_ref @_T018pointer_conversion11sideEffect1SiyF
  // CHECK:   [[RESULT1:%.*]] = apply [[SIDE1]]()
  // CHECK:   [[T0:%.*]] = select_enum [[COPY]]
  //   FIXME: this should really go somewhere that will make nil, not some(nil)
  // CHECK:   cond_br [[T0]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]:
  // CHECK:   [[SOME_VALUE:%.*]] = unchecked_enum_data [[COPY]]
  // CHECK:   [[T0:%.*]] = select_enum [[SOME_VALUE]]
  // CHECK:   cond_br [[T0]], [[SOME_SOME_BB:bb[0-9]+]], [[SOME_NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_NONE_BB]]:
  // CHECK:   destroy_value [[SOME_VALUE]]
  // CHECK:   br [[SOME_NONE_BB2:bb[0-9]+]]
  // CHECK: [[SOME_SOME_BB]]:
  // CHECK:   [[SOME_SOME_VALUE:%.*]] = unchecked_enum_data [[SOME_VALUE]]
  // CHECK:   [[CONVERT:%.*]] = function_ref @_T0s40_convertConstStringToUTF8PointerArgumentyXlSg_xtSSs01_F0RzlF
  // CHECK:   [[TEMP:%.*]] = alloc_stack $UnsafeRawPointer
  // CHECK:   [[OWNER:%.*]] = apply [[CONVERT]]<UnsafeRawPointer>([[TEMP:%.*]], [[SOME_SOME_VALUE]])
  // CHECK:   [[PTR:%.*]] = load [trivial] [[TEMP]]
  // CHECK:   [[DEP:%.*]] = mark_dependence [[PTR]] : $UnsafeRawPointer on [[OWNER]]
  // CHECK:   [[OPTPTR:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.some!enumelt.1, [[DEP]]
  // CHECK:   dealloc_stack [[TEMP]]
  // CHECK:   br [[SOME_SOME_CONT_BB:bb[0-9]+]]([[OPTPTR]] : $Optional<UnsafeRawPointer>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_SOME_CONT_BB]]([[OPTPTR:%.*]] : $Optional<UnsafeRawPointer>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTDEP:%.*]] = mark_dependence [[OPTPTR]] : $Optional<UnsafeRawPointer> on [[OWNER]]
  // CHECK:   [[OPTOPTPTR:%.*]] = enum $Optional<Optional<UnsafeRawPointer>>, #Optional.some!enumelt.1, [[OPTDEP]]
  // CHECK:   br [[SOME_CONT_BB:bb[0-9]+]]([[OPTOPTPTR]] : $Optional<Optional<UnsafeRawPointer>>, [[OWNER]] : $Optional<AnyObject>)
  // CHECK: [[SOME_CONT_BB]]([[OPTOPTPTR:%.*]] : $Optional<Optional<UnsafeRawPointer>>, [[OWNER:%.*]] : $Optional<AnyObject>):
  // CHECK:   [[OPTOPTDEP:%.*]] = mark_dependence [[OPTOPTPTR]] : $Optional<Optional<UnsafeRawPointer>> on [[OWNER]]
  // CHECK:   apply [[TAKES]]([[OPTOPTDEP]], [[RESULT1]])
  // CHECK:   destroy_value [[OWNER]]
  // CHECK:   end_borrow [[BORROW]]
  // CHECK:   destroy_value %0
  // CHECK: [[SOME_NONE_BB2]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<UnsafeRawPointer>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_SOME_CONT_BB]]([[NO_VALUE]] : $Optional<UnsafeRawPointer>, [[NO_OWNER]] : $Optional<AnyObject>)
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[NO_VALUE:%.*]] = enum $Optional<Optional<UnsafeRawPointer>>, #Optional.none
  // CHECK:   [[NO_OWNER:%.*]] = enum $Optional<AnyObject>, #Optional.none
  // CHECK:   br [[SOME_CONT_BB]]([[NO_VALUE]] : $Optional<Optional<UnsafeRawPointer>>, [[NO_OWNER]] : $Optional<AnyObject>)
  takesOptOptConstRawPointer(string, and: sideEffect1())
}
