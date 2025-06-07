
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk-nosource -I %t) -Xllvm -sil-print-types -module-name foreign_errors -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import errors

// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors5test0yyKF : $@convention(thin) () -> @error any Error
func test0() throws {
  //   Create a strong temporary holding nil before we perform any further parts of function emission.
  // CHECK: [[ERR_TEMP0:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
  // CHECK: inject_enum_addr [[ERR_TEMP0]] : $*Optional<NSError>, #Optional.none!enumelt

  // CHECK: [[SELF:%.*]] = metatype $@objc_metatype ErrorProne.Type
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $@objc_metatype ErrorProne.Type, #ErrorProne.fail!foreign : (ErrorProne.Type) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> ObjCBool

  //   Create an unmanaged temporary, copy into it, and make a AutoreleasingUnsafeMutablePointer.
  // CHECK: [[ERR_TEMP1:%.*]] = alloc_stack $@sil_unmanaged Optional<NSError>
  // CHECK: [[T0:%.*]] = load_borrow [[ERR_TEMP0]]
  // CHECK: [[T1:%.*]] = ref_to_unmanaged [[T0]]
  // CHECK: store [[T1]] to [trivial] [[ERR_TEMP1]]
  // CHECK: address_to_pointer [stack_protection] [[ERR_TEMP1]]

  //   Call the method.
  // CHECK: [[RESULT:%.*]] = apply [[METHOD]]({{.*}}, [[SELF]])

  //   Writeback to the first temporary.
  //
  //   NOTE: We need to a mark dependence here to ensure that the destroy
  //   associated with the assign to ERR_TEMP0 is not hoisted above the copy_value
  //   of T1_COPY. If we were to allow that, we could introduce a lifetime gap
  //   causing potentially use after frees.
  //
  // CHECK: [[T0:%.*]] = load [trivial] [[ERR_TEMP1]]
  // CHECK: [[T1:%.*]] = unmanaged_to_ref [[T0]]
  // CHECK: [[T1_COPY:%.*]] = copy_value [[T1]]
  // CHECK: [[T1_COPY_DEP:%.*]] = mark_dependence [[T1_COPY]] : $Optional<NSError> on [[ERR_TEMP0]]
  // CHECK: assign [[T1_COPY_DEP]] to [[ERR_TEMP0]]

  //   Pull out the boolean value and compare it to zero.
  // CHECK: [[BOOL_OR_INT:%.*]] = struct_extract [[RESULT]]
  // CHECK: [[RAW_VALUE:%.*]] = struct_extract [[BOOL_OR_INT]]
  //   On some platforms RAW_VALUE will be compared against 0; on others it's
  //   already an i1 (bool) and those instructions will be skipped. Just do a
  //   looser check.
  // CHECK: cond_br {{%.+}}, [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
  try ErrorProne.fail()

  //   Normal path: fall out and return.
  // CHECK: [[NORMAL_BB]]:
  // CHECK: return
  
  //   Error path: fall out and rethrow.
  // CHECK: [[ERROR_BB]]:
  // CHECK: [[T0:%.*]] = load [take] [[ERR_TEMP0]]
  // CHECK: [[T1:%.*]] = function_ref @$s10Foundation22_convertNSErrorToErrorys0E0_pSo0C0CSgF : $@convention(thin) (@guaranteed Optional<NSError>) -> @owned any Error
  // CHECK: [[T2:%.*]] = apply [[T1]]([[T0]])
  // CHECK: "willThrow"([[T2]] : $any Error)
  // CHECK: throw [[T2]] : $any Error
}

extension NSObject {
  @objc func abort() throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
// CHECK-LABEL: sil private [thunk] [ossa] @$sSo8NSObjectC14foreign_errorsE5abortyyKFTo : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> ObjCBool
// CHECK: [[T0:%.*]] = function_ref @$sSo8NSObjectC14foreign_errorsE5abortyyKF : $@convention(method) (@guaranteed NSObject) -> @error any Error
// CHECK: try_apply [[T0]](
// CHECK: bb1(
// CHECK:   [[BITS:%.*]] = integer_literal $Builtin.Int{{[18]}}, {{1|-1}}
// CHECK:   [[VALUE:%.*]] = struct ${{Bool|UInt8}} ([[BITS]] : $Builtin.Int{{[18]}})
// CHECK:   [[BOOL:%.*]] = struct $ObjCBool ([[VALUE]] : ${{Bool|UInt8}})
// CHECK:   br bb6([[BOOL]] : $ObjCBool)
// CHECK: bb2([[ERR:%.*]] : @owned $any Error):
// CHECK:   switch_enum %0 : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, case #Optional.some!enumelt: bb3, case #Optional.none!enumelt: bb4
// CHECK: bb3([[UNWRAPPED_OUT:%.+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>):
// CHECK:   [[T0:%.*]] = function_ref @$s10Foundation22_convertErrorToNSErrorySo0E0Cs0C0_pF : $@convention(thin) (@guaranteed any Error) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.some!enumelt, [[T1]] : $NSError
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [init] [[TEMP]]
// CHECK:   [[SETTER:%.*]] = function_ref @$sSA7pointeexvs :
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]], [[UNWRAPPED_OUT]])
// CHECK:   dealloc_stack [[TEMP]]
// CHECK:   br bb5
// CHECK: bb4:
// CHECK:   destroy_value [[ERR]] : $any Error
// CHECK:   br bb5
// CHECK: bb5:
// CHECK:   [[BITS:%.*]] = integer_literal $Builtin.Int{{[18]}}, 0
// CHECK:   [[VALUE:%.*]] = struct ${{Bool|UInt8}} ([[BITS]] : $Builtin.Int{{[18]}})
// CHECK:   [[BOOL:%.*]] = struct $ObjCBool ([[VALUE]] : ${{Bool|UInt8}})
// CHECK:   br bb6([[BOOL]] : $ObjCBool)
// CHECK: bb6([[BOOL:%.*]] : $ObjCBool):
// CHECK:   return [[BOOL]] : $ObjCBool

  @objc func badDescription() throws -> String {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
// CHECK-LABEL: sil private [thunk] [ossa] @$sSo8NSObjectC14foreign_errorsE14badDescriptionSSyKFTo : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> @autoreleased Optional<NSString> {
// CHECK: bb0([[UNOWNED_ARG0:%.*]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[UNOWNED_ARG1:%.*]] : @unowned $NSObject):
// CHECK: [[ARG1:%.*]] = copy_value [[UNOWNED_ARG1]]
// CHECK: [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK: [[T0:%.*]] = function_ref @$sSo8NSObjectC14foreign_errorsE14badDescriptionSSyKF : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error any Error)
// CHECK: try_apply [[T0]]([[BORROWED_ARG1]]) : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error any Error), normal [[NORMAL_BB:bb[0-9][0-9]*]], error [[ERROR_BB:bb[0-9][0-9]*]]
//
// CHECK: [[NORMAL_BB]]([[RESULT:%.*]] : @owned $String):
// CHECK:   [[T0:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   [[BORROWED_RESULT:%.*]] = begin_borrow [[RESULT]]
// CHECK:   [[T1:%.*]] = apply [[T0]]([[BORROWED_RESULT]])
// CHECK:   end_borrow [[BORROWED_RESULT]]
// CHECK:   [[T2:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt, [[T1]] : $NSString
// CHECK:   destroy_value [[RESULT]]  
// CHECK:   br bb6([[T2]] : $Optional<NSString>)
//
// CHECK: [[ERROR_BB]]([[ERR:%.*]] : @owned $any Error):
// CHECK:   switch_enum [[UNOWNED_ARG0]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9][0-9]*]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9][0-9]*]]
//
// CHECK: [[SOME_BB]]([[UNWRAPPED_OUT:%.+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>):
// CHECK:   [[T0:%.*]] = function_ref @$s10Foundation22_convertErrorToNSErrorySo0E0Cs0C0_pF : $@convention(thin) (@guaranteed any Error) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.some!enumelt, [[T1]] : $NSError
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [init] [[TEMP]]
// CHECK:   [[SETTER:%.*]] = function_ref @$sSA7pointeexvs :
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]], [[UNWRAPPED_OUT]])
// CHECK:   dealloc_stack [[TEMP]]
// CHECK:   br bb5
//
// CHECK: [[NONE_BB]]:
// CHECK:   destroy_value [[ERR]] : $any Error
// CHECK:   br bb5
//
// CHECK: bb5:
// CHECK:   [[T0:%.*]] = enum $Optional<NSString>, #Optional.none!enumelt
// CHECK:   br bb6([[T0]] : $Optional<NSString>)
//
// CHECK: bb6([[T0:%.*]] : @owned $Optional<NSString>):
// CHECK:   end_borrow [[BORROWED_ARG1]]
// CHECK:   destroy_value [[ARG1]]
// CHECK:   return [[T0]] : $Optional<NSString>

// CHECK-LABEL: sil private [thunk] [ossa] @$sSo8NSObjectC14foreign_errorsE7takeIntyySiKFTo : $@convention(objc_method) (Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> ObjCBool
// CHECK: bb0([[I:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[SELF:%[0-9]+]] : @unowned $NSObject)
  @objc func takeInt(_ i: Int) throws { }

// CHECK-LABEL: sil private [thunk] [ossa] @$sSo8NSObjectC14foreign_errorsE10takeDouble_3int7closureySd_S3iXEtKFTo : $@convention(objc_method) (Double, Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @convention(block) @noescape (Int) -> Int, NSObject) -> ObjCBool
// CHECK: bb0([[D:%[0-9]+]] : $Double, [[INT:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[CLOSURE:%[0-9]+]] : @unowned $@convention(block) @noescape (Int) -> Int, [[SELF:%[0-9]+]] : @unowned $NSObject):
  @objc func takeDouble(_ d: Double, int: Int, closure: (Int) -> Int) throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
}

let fn = ErrorProne.fail
// CHECK-LABEL: sil private [ossa] @$s14foreign_errors2fnyyKcvpfiyyKcfu_ : $@convention(thin) () -> @error any Error

func testArgs() throws {
  try ErrorProne.consume(nil)
}
// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors8testArgsyyKF : $@convention(thin) () -> @error any Error
// CHECK:   debug_value undef : $any Error, var, name "$error", argno 1
// CHECK:   objc_method {{.*}} : $@objc_metatype ErrorProne.Type, #ErrorProne.consume!foreign : (ErrorProne.Type) -> (Any?) throws -> (), $@convention(objc_method) (Optional<AnyObject>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> ObjCBool

func testBridgedResult() throws {
  let array = try ErrorProne.collection(withCount: 0)
}
// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors17testBridgedResultyyKF : $@convention(thin) () -> @error any Error {
// CHECK:   debug_value undef : $any Error, var, name "$error", argno 1
// CHECK:   objc_method {{.*}} : $@objc_metatype ErrorProne.Type, #ErrorProne.collection!foreign : (ErrorProne.Type) -> (Int) throws -> [Any], $@convention(objc_method) (Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> @autoreleased Optional<NSArray>
// CHECK: } // end sil function '$s14foreign_errors17testBridgedResultyyKF'

// rdar://20861374
// Clear out the self box before delegating.
class VeryErrorProne : ErrorProne {
  init(withTwo two: AnyObject?) throws {
    try super.init(one: two)
  }
}

// SEMANTIC SIL TODO: _TFC14foreign_errors14VeryErrorPronec has a lot more going
// on than is being tested here, we should consider adding FileCheck tests for
// it.

// CHECK-LABEL:    sil hidden [ossa] @$s14foreign_errors14VeryErrorProneC7withTwoACyXlSg_tKcfc :
// CHECK:    bb0([[ARG1:%.*]] : @owned $Optional<AnyObject>, [[ARG2:%.*]] : @owned $VeryErrorProne):
// CHECK:      [[BOX:%.*]] = alloc_box ${ var VeryErrorProne }
// CHECK:      [[MARKED_BOX:%.*]] = mark_uninitialized [derivedself] [[BOX]]
// CHECK:      [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_BOX]]
// CHECK:      [[PB:%.*]] = project_box [[LIFETIME]]
// CHECK:      store [[ARG2]] to [init] [[PB]]
// CHECK:      [[T0:%.*]] = load [take] [[PB]]
// CHECK-NEXT: [[T1:%.*]] = upcast [[T0]] : $VeryErrorProne to $ErrorProne
// CHECK:      [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:      [[ARG1_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
// CHECK-NOT:  [[BOX]]{{^[0-9]}}
// CHECK-NOT:  [[PB]]{{^[0-9]}}
// CHECK: [[BORROWED_T1:%.*]] = begin_borrow [[T1]]
// CHECK-NEXT: [[DOWNCAST_BORROWED_T1:%.*]] = unchecked_ref_cast [[BORROWED_T1]] : $ErrorProne to $VeryErrorProne
// CHECK-NEXT: [[T2:%.*]] = objc_super_method [[DOWNCAST_BORROWED_T1]] : $VeryErrorProne, #ErrorProne.init!initializer.foreign : (ErrorProne.Type) -> (Any?) throws -> ErrorProne, $@convention(objc_method) (Optional<AnyObject>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @owned ErrorProne) -> @owned Optional<ErrorProne>
// CHECK:      end_borrow [[BORROWED_T1]]
// CHECK:      apply [[T2]]([[ARG1_COPY]], {{%.*}}, [[T1]])
// CHECK: } // end sil function '$s14foreign_errors14VeryErrorProneC7withTwoACyXlSg_tKcfc'

// rdar://21051021
// CHECK: sil hidden [ossa] @$s14foreign_errors12testProtocolyySo010ErrorProneD0_pKF : $@convention(thin) (@guaranteed any ErrorProneProtocol) -> @error any Error
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $any ErrorProneProtocol):
func testProtocol(_ p: ErrorProneProtocol) throws {
  // CHECK:   [[T0:%.*]] = open_existential_ref [[ARG0]] : $any ErrorProneProtocol to $[[OPENED:@opened\(.*, any ErrorProneProtocol\) Self]]
  // CHECK:   [[T1:%.*]] = objc_method [[T0]] : $[[OPENED]], #ErrorProneProtocol.obliterate!foreign : {{.*}}
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, τ_0_0) -> ObjCBool
  try p.obliterate()

  // CHECK:   [[T0:%.*]] = open_existential_ref [[ARG0]] : $any ErrorProneProtocol to $[[OPENED:@opened\(.*, any ErrorProneProtocol\) Self]]
  // CHECK:   [[T1:%.*]] = objc_method [[T0]] : $[[OPENED]], #ErrorProneProtocol.invigorate!foreign : {{.*}}
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, {{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>, τ_0_0) -> ObjCBool
  try p.invigorate(callback: {})
}

// rdar://21144509 - Ensure that overrides of replace-with-() imports are possible.
class ExtremelyErrorProne : ErrorProne {
  override func conflict3(_ obj: Any, error: ()) throws {}
}
// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors19ExtremelyErrorProneC9conflict3_5erroryyp_yttKF
// CHECK-LABEL: sil private [thunk] [ossa] @$s14foreign_errors19ExtremelyErrorProneC9conflict3_5erroryyp_yttKFTo : $@convention(objc_method) (AnyObject, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, ExtremelyErrorProne) -> ObjCBool

// These conventions are usable because of swift_error. rdar://21715350
func testNonNilError() throws -> Float {
  return try ErrorProne.bounce()
}
// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors15testNonNilErrorSfyKF :
// CHECK:   [[OPTERR:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
// CHECK:   [[T0:%.*]] = metatype $@objc_metatype ErrorProne.Type
// CHECK:   [[T1:%.*]] = objc_method [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.bounce!foreign : (ErrorProne.Type) -> () throws -> Float, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Float
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   assign {{%.*}} to [[OPTERR]]
// CHECK:   [[T0:%.*]] = load [take] [[OPTERR]]
// CHECK:   switch_enum [[T0]] : $Optional<NSError>, case #Optional.some!enumelt: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return [[RESULT]]
// CHECK: [[ERROR_BB]]

func testPreservedResult() throws -> CInt {
  return try ErrorProne.ounce()
}
// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors19testPreservedResults5Int32VyKF
// CHECK:   [[OPTERR:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
// CHECK:   [[T0:%.*]] = metatype $@objc_metatype ErrorProne.Type
// CHECK:   [[T1:%.*]] = objc_method [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.ounce!foreign : (ErrorProne.Type) -> () throws -> Int32, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int32
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   [[T0:%.*]] = struct_extract [[RESULT]]
// CHECK:   [[T1:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
// CHECK:   [[T2:%.*]] = builtin "cmp_ne_Int32"([[T0]] : $[[PRIM]], [[T1]] : $[[PRIM]])
// CHECK:   cond_br [[T2]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return [[RESULT]]
// CHECK: [[ERROR_BB]]

func testPreservedResultBridged() throws -> Int {
  return try ErrorProne.ounceWord()
}

// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors26testPreservedResultBridgedSiyKF
// CHECK:   [[OPTERR:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
// CHECK:   [[T0:%.*]] = metatype $@objc_metatype ErrorProne.Type
// CHECK:   [[T1:%.*]] = objc_method [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.ounceWord!foreign : (ErrorProne.Type) -> () throws -> Int, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   [[T0:%.*]] = struct_extract [[RESULT]]
// CHECK:   [[T1:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
// CHECK:   [[T2:%.*]] = builtin "cmp_ne_Int{{.*}}"([[T0]] : $[[PRIM]], [[T1]] : $[[PRIM]])
// CHECK:   cond_br [[T2]], [[NORMAL_BB:bb[0-9]+]], [[ERROR_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return [[RESULT]]
// CHECK: [[ERROR_BB]]

func testPreservedResultInverted() throws {
  try ErrorProne.once()
}

// CHECK-LABEL: sil hidden [ossa] @$s14foreign_errors27testPreservedResultInvertedyyKF
// CHECK:   [[OPTERR:%.*]] = alloc_stack [dynamic_lifetime] $Optional<NSError>
// CHECK:   [[T0:%.*]] = metatype $@objc_metatype ErrorProne.Type
// CHECK:   [[T1:%.*]] = objc_method [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.once!foreign : (ErrorProne.Type) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int32
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   [[T0:%.*]] = struct_extract [[RESULT]]
// CHECK:   [[T1:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
// CHECK:   [[T2:%.*]] = builtin "cmp_ne_Int32"([[T0]] : $[[PRIM]], [[T1]] : $[[PRIM]])
// CHECK:   cond_br [[T2]], [[ERROR_BB:bb[0-9]+]], [[NORMAL_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return {{%.+}} : $()
// CHECK: [[ERROR_BB]]

// Make sure that we do not crash when emitting the error value here.
//
// TODO: Add some additional filecheck tests.
extension NSURL {
  func resourceValue<T>(forKey key: String) -> T? {
    var prop: AnyObject? = nil
    _ = try? self.getResourceValue(&prop, forKey: key)
    return prop as? T
  }
}
