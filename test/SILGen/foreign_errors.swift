// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-silgen -parse-as-library %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import errors

// CHECK: sil hidden @_TF14foreign_errors5test0FzT_T_ : $@convention(thin) () -> @error Error
func test0() throws {
  // CHECK: [[SELF:%.*]] = metatype $@thick ErrorProne.Type
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $@thick ErrorProne.Type, #ErrorProne.fail!1.foreign : (ErrorProne.Type) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> ObjCBool
  // CHECK: [[OBJC_SELF:%.*]] = thick_to_objc_metatype [[SELF]]

  //   Create a strong temporary holding nil.
  // CHECK: [[ERR_TEMP0:%.*]] = alloc_stack $Optional<NSError>
  // CHECK: inject_enum_addr [[ERR_TEMP0]] : $*Optional<NSError>, #Optional.none!enumelt
  //   Create an unmanaged temporary, copy into it, and make a AutoreleasingUnsafeMutablePointer.
  // CHECK: [[ERR_TEMP1:%.*]] = alloc_stack $@sil_unmanaged Optional<NSError>
  // CHECK: [[T0:%.*]] = load_borrow [[ERR_TEMP0]]
  // CHECK: [[T1:%.*]] = ref_to_unmanaged [[T0]]
  // CHECK: store [[T1]] to [trivial] [[ERR_TEMP1]]
  // CHECK: address_to_pointer [[ERR_TEMP1]]

  //   Call the method.
  // CHECK: [[RESULT:%.*]] = apply [[METHOD]]({{.*}}, [[OBJC_SELF]])

  //   Writeback to the first temporary.
  // CHECK: [[T0:%.*]] = load [trivial] [[ERR_TEMP1]]
  // CHECK: [[T1:%.*]] = unmanaged_to_ref [[T0]]
  // CHECK: [[T1_COPY:%.*]] = copy_value [[T1]]
  // CHECK: assign [[T1_COPY]] to [[ERR_TEMP0]]

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
  // CHECK: [[T1:%.*]] = function_ref @swift_convertNSErrorToError : $@convention(thin) (@owned Optional<NSError>) -> @owned Error
  // CHECK: [[T2:%.*]] = apply [[T1]]([[T0]])
  // CHECK: throw [[T2]] : $Error
}

extension NSObject {
  @objc func abort() throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
// CHECK-LABEL: sil hidden [thunk] @_TToFE14foreign_errorsCSo8NSObject5abort{{.*}} : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> ObjCBool
// CHECK: [[T0:%.*]] = function_ref @_TFE14foreign_errorsCSo8NSObject5abort{{.*}} : $@convention(method) (@guaranteed NSObject) -> @error Error
// CHECK: try_apply [[T0]](
// CHECK: bb1(
// CHECK:   [[BITS:%.*]] = integer_literal $Builtin.Int{{[18]}}, {{1|-1}}
// CHECK:   [[VALUE:%.*]] = struct ${{Bool|UInt8}} ([[BITS]] : $Builtin.Int{{[18]}})
// CHECK:   [[BOOL:%.*]] = struct $ObjCBool ([[VALUE]] : ${{Bool|UInt8}})
// CHECK:   br bb6([[BOOL]] : $ObjCBool)
// CHECK: bb2([[ERR:%.*]] : $Error):
// CHECK:   switch_enum %0 : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, case #Optional.some!enumelt.1: bb3, case #Optional.none!enumelt: bb4
// CHECK: bb3([[UNWRAPPED_OUT:%.+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorToNSError : $@convention(thin) (@owned Error) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVs33AutoreleasingUnsafeMutablePointers7pointeex :
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [init] [[TEMP]]
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]], [[UNWRAPPED_OUT]])
// CHECK:   dealloc_stack [[TEMP]]
// CHECK:   br bb5
// CHECK: bb4:
// CHECK:   destroy_value [[ERR]] : $Error
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
// CHECK-LABEL: sil hidden [thunk] @_TToFE14foreign_errorsCSo8NSObject14badDescription{{.*}} : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> @autoreleased Optional<NSString> {
// CHECK: bb0([[UNOWNED_ARG0:%.*]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[UNOWNED_ARG1:%.*]] : $NSObject):
// CHECK: [[ARG1:%.*]] = copy_value [[UNOWNED_ARG1]]
// CHECK: [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK: [[T0:%.*]] = function_ref @_TFE14foreign_errorsCSo8NSObject14badDescription{{.*}} : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error Error)
// CHECK: try_apply [[T0]]([[BORROWED_ARG1]]) : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error Error), normal [[NORMAL_BB:bb[0-9][0-9]*]], error [[ERROR_BB:bb[0-9][0-9]*]]
//
// CHECK: [[NORMAL_BB]]([[RESULT:%.*]] : $String):
// CHECK:   [[T0:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveCfT_CSo8NSString : $@convention(method) (@guaranteed String) -> @owned NSString
// CHECK:   [[BORROWED_RESULT:%.*]] = begin_borrow [[RESULT]]
// CHECK:   [[T1:%.*]] = apply [[T0]]([[BORROWED_RESULT]])
// CHECK:   [[T2:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[T1]] : $NSString
// CHECK:   end_borrow [[BORROWED_RESULT]] from [[RESULT]]
// CHECK:   destroy_value [[RESULT]]  
// CHECK:   br bb6([[T2]] : $Optional<NSString>)
//
// CHECK: [[ERROR_BB]]([[ERR:%.*]] : $Error):
// CHECK:   switch_enum [[UNOWNED_ARG0]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9][0-9]*]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9][0-9]*]]
//
// CHECK: [[SOME_BB]]([[UNWRAPPED_OUT:%.+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorToNSError : $@convention(thin) (@owned Error) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVs33AutoreleasingUnsafeMutablePointers7pointeex :
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [init] [[TEMP]]
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]], [[UNWRAPPED_OUT]])
// CHECK:   dealloc_stack [[TEMP]]
// CHECK:   br bb5
//
// CHECK: [[NONE_BB]]:
// CHECK:   destroy_value [[ERR]] : $Error
// CHECK:   br bb5
//
// CHECK: bb5:
// CHECK:   [[T0:%.*]] = enum $Optional<NSString>, #Optional.none!enumelt
// CHECK:   br bb6([[T0]] : $Optional<NSString>)
//
// CHECK: bb6([[T0:%.*]] : $Optional<NSString>):
// CHECK:   end_borrow [[BORROWED_ARG1]] from [[ARG1]]
// CHECK:   destroy_value [[ARG1]]
// CHECK:   return [[T0]] : $Optional<NSString>

// CHECK-LABEL: sil hidden [thunk] @_TToFE14foreign_errorsCSo8NSObject7takeInt{{.*}} : $@convention(objc_method) (Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSObject) -> ObjCBool
// CHECK: bb0([[I:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[SELF:%[0-9]+]] : $NSObject)
  @objc func takeInt(_ i: Int) throws { }

// CHECK-LABEL: sil hidden [thunk] @_TToFE14foreign_errorsCSo8NSObject10takeDouble{{.*}} : $@convention(objc_method) (Double, Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @convention(block) (Int) -> Int, NSObject) -> ObjCBool
// CHECK: bb0([[D:%[0-9]+]] : $Double, [[INT:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, [[CLOSURE:%[0-9]+]] : $@convention(block) (Int) -> Int, [[SELF:%[0-9]+]] : $NSObject):
  @objc func takeDouble(_ d: Double, int: Int, closure: (Int) -> Int) throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
}

let fn = ErrorProne.fail
// CHECK-LABEL: sil shared [thunk] @_TTOZFCSo10ErrorProne4fail{{.*}} : $@convention(thin) (@thick ErrorProne.Type) -> @owned @callee_owned () -> @error Error
// CHECK:      [[T0:%.*]] = function_ref @_TTOZFCSo10ErrorProne4fail{{.*}} : $@convention(method) (@thick ErrorProne.Type) -> @error Error
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[T0]](%0)
// CHECK-NEXT: return [[T1]]

// CHECK-LABEL: sil shared [thunk] @_TTOZFCSo10ErrorProne4fail{{.*}} : $@convention(method) (@thick ErrorProne.Type) -> @error Error {
// CHECK:      [[SELF:%.*]] = thick_to_objc_metatype %0 : $@thick ErrorProne.Type to $@objc_metatype ErrorProne.Type
// CHECK:      [[METHOD:%.*]] = class_method [volatile] [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.fail!1.foreign : (ErrorProne.Type) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> ObjCBool
// CHECK:      [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:      [[RESULT:%.*]] = apply [[METHOD]]({{%.*}}, [[SELF]])
// CHECK:      cond_br
// CHECK:      return
// CHECK:      [[T0:%.*]] = load [take] [[TEMP]]
// CHECK:      [[T1:%.*]] = apply {{%.*}}([[T0]])
// CHECK:      throw [[T1]]

func testArgs() throws {
  try ErrorProne.consume(nil)
}
// CHECK: sil hidden @_TF14foreign_errors8testArgsFzT_T_ : $@convention(thin) () -> @error Error
// CHECK:   debug_value undef : $Error, var, name "$error", argno 1
// CHECK:   class_method [volatile] %1 : $@thick ErrorProne.Type, #ErrorProne.consume!1.foreign : (ErrorProne.Type) -> (Any!) throws -> (), $@convention(objc_method) (Optional<AnyObject>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> ObjCBool

func testBridgedResult() throws {
  let array = try ErrorProne.collection(withCount: 0)
}
// CHECK-LABEL: sil hidden @_TF14foreign_errors17testBridgedResultFzT_T_ : $@convention(thin) () -> @error Error {
// CHECK:   debug_value undef : $Error, var, name "$error", argno 1
// CHECK:   class_method [volatile] %1 : $@thick ErrorProne.Type, #ErrorProne.collection!1.foreign : (ErrorProne.Type) -> (Int) throws -> [Any], $@convention(objc_method) (Int, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> @autoreleased Optional<NSArray>

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

// CHECK-LABEL:    sil hidden @_TFC14foreign_errors14VeryErrorPronec{{.*}}
// CHECK:    bb0([[ARG1:%.*]] : $Optional<AnyObject>, [[ARG2:%.*]] : $VeryErrorProne):
// CHECK:      [[BOX:%.*]] = alloc_box ${ var VeryErrorProne }
// CHECK:      [[PB:%.*]] = project_box [[BOX]]
// CHECK:      [[MARKED_BOX:%.*]] = mark_uninitialized [derivedself] [[PB]]
// CHECK:      store [[ARG2]] to [init] [[MARKED_BOX]]
// CHECK:      [[T0:%.*]] = load [take] [[MARKED_BOX]]
// CHECK-NEXT: [[T1:%.*]] = upcast [[T0]] : $VeryErrorProne to $ErrorProne
// CHECK-NEXT: [[T2:%.*]] = super_method [volatile] [[T0]] : $VeryErrorProne, #ErrorProne.init!initializer.1.foreign : (ErrorProne.Type) -> (Any?) throws -> ErrorProne, $@convention(objc_method) (Optional<AnyObject>, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @owned ErrorProne) -> @owned Optional<ErrorProne>
// CHECK:      [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
// CHECK:      [[ARG1_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
// CHECK-NOT:  [[BOX]]{{^[0-9]}}
// CHECK-NOT:  [[MARKED_BOX]]{{^[0-9]}}
// CHECK:      apply [[T2]]([[ARG1_COPY]], {{%.*}}, [[T1]])

// rdar://21051021
// CHECK: sil hidden @_TF14foreign_errors12testProtocolFzPSo18ErrorProneProtocol_T_ : $@convention(thin) (@owned ErrorProneProtocol) -> @error Error
// CHECK: bb0([[ARG0:%.*]] : $ErrorProneProtocol):
func testProtocol(_ p: ErrorProneProtocol) throws {
  // CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK:   [[T0:%.*]] = open_existential_ref [[BORROWED_ARG0]] : $ErrorProneProtocol to $[[OPENED:@opened(.*) ErrorProneProtocol]]
  // CHECK:   [[T1:%.*]] = witness_method [volatile] $[[OPENED]], #ErrorProneProtocol.obliterate!1.foreign : {{.*}}, [[T0]] : $[[OPENED]] :
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, τ_0_0) -> ObjCBool
  try p.obliterate()

  // CHECK:   [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK:   [[T0:%.*]] = open_existential_ref [[BORROWED_ARG0]] : $ErrorProneProtocol to $[[OPENED:@opened(.*) ErrorProneProtocol]]
  // CHECK:   [[T1:%.*]] = witness_method [volatile] $[[OPENED]], #ErrorProneProtocol.invigorate!1.foreign : {{.*}}, [[T0]] : $[[OPENED]] :
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, {{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, Optional<@convention(block) () -> ()>, τ_0_0) -> ObjCBool
  try p.invigorate(callback: {})
}

// rdar://21144509 - Ensure that overrides of replace-with-() imports are possible.
class ExtremelyErrorProne : ErrorProne {
  override func conflict3(_ obj: Any, error: ()) throws {}
}
// CHECK: sil hidden @_TFC14foreign_errors19ExtremelyErrorProne9conflict3{{.*}}
// CHECK: sil hidden [thunk] @_TToFC14foreign_errors19ExtremelyErrorProne9conflict3{{.*}} : $@convention(objc_method) (AnyObject, Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, ExtremelyErrorProne) -> ObjCBool

// These conventions are usable because of swift_error. rdar://21715350
func testNonNilError() throws -> Float {
  return try ErrorProne.bounce()
}
// CHECK: sil hidden @_TF14foreign_errors15testNonNilErrorFzT_Sf :
// CHECK:   [[T0:%.*]] = metatype $@thick ErrorProne.Type
// CHECK:   [[T1:%.*]] = class_method [volatile] [[T0]] : $@thick ErrorProne.Type, #ErrorProne.bounce!1.foreign : (ErrorProne.Type) -> () throws -> Float, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Float
// CHECK:   [[OPTERR:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   assign {{%.*}} to [[OPTERR]]
// CHECK:   [[T0:%.*]] = load [take] [[OPTERR]]
// CHECK:   switch_enum [[T0]] : $Optional<NSError>, case #Optional.some!enumelt.1: [[ERROR_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NORMAL_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return [[RESULT]]
// CHECK: [[ERROR_BB]]

func testPreservedResult() throws -> CInt {
  return try ErrorProne.ounce()
}
// CHECK: sil hidden @_TF14foreign_errors19testPreservedResultFzT_Vs5Int32
// CHECK:   [[T0:%.*]] = metatype $@thick ErrorProne.Type
// CHECK:   [[T1:%.*]] = class_method [volatile] [[T0]] : $@thick ErrorProne.Type, #ErrorProne.ounce!1.foreign : (ErrorProne.Type) -> () throws -> Int32, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int32
// CHECK:   [[OPTERR:%.*]] = alloc_stack $Optional<NSError>
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

// CHECK-LABEL: sil hidden @_TF14foreign_errors26testPreservedResultBridgedFzT_Si
// CHECK:   [[T0:%.*]] = metatype $@thick ErrorProne.Type
// CHECK:   [[T1:%.*]] = class_method [volatile] [[T0]] : $@thick ErrorProne.Type, #ErrorProne.ounceWord!1.foreign : (ErrorProne.Type) -> () throws -> Int, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int
// CHECK:   [[OPTERR:%.*]] = alloc_stack $Optional<NSError>
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

// CHECK-LABEL: sil hidden @_TF14foreign_errors27testPreservedResultInvertedFzT_T_
// CHECK:   [[T0:%.*]] = metatype $@thick ErrorProne.Type
// CHECK:   [[T1:%.*]] = class_method [volatile] [[T0]] : $@thick ErrorProne.Type, #ErrorProne.once!1.foreign : (ErrorProne.Type) -> () throws -> (), $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @objc_metatype ErrorProne.Type) -> Int32
// CHECK:   [[OPTERR:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   [[RESULT:%.*]] = apply [[T1]](
// CHECK:   [[T0:%.*]] = struct_extract [[RESULT]]
// CHECK:   [[T1:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
// CHECK:   [[T2:%.*]] = builtin "cmp_ne_Int32"([[T0]] : $[[PRIM]], [[T1]] : $[[PRIM]])
// CHECK:   cond_br [[T2]], [[ERROR_BB:bb[0-9]+]], [[NORMAL_BB:bb[0-9]+]]
// CHECK: [[NORMAL_BB]]:
// CHECK-NOT: destroy_value
// CHECK:   return {{%.+}} : $()
// CHECK: [[ERROR_BB]]
