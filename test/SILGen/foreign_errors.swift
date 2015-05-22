// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -parse-as-library %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import errors

// CHECK: sil hidden @_TF14foreign_errors5test0FzT_T_ : $@convention(thin) () -> @error ErrorType
func test0() throws {
  // CHECK: [[SELF:%.*]] = metatype $@thick ErrorProne.Type
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $@thick ErrorProne.Type, #ErrorProne.fail!1.foreign : ErrorProne.Type -> () throws -> () , $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @objc_metatype ErrorProne.Type) -> Bool
  // CHECK: [[OBJC_SELF:%.*]] = thick_to_objc_metatype [[SELF]]

  //   Create a strong temporary holding nil.
  // CHECK: [[ERR_TEMP0:%.*]] = alloc_stack $Optional<NSError>
  // CHECK: inject_enum_addr [[ERR_TEMP0]]#1 : $*Optional<NSError>, #Optional.None!enumelt
  //   Create an unmanaged temporary, copy into it, and make a AutoreleasingUnsafeMutablePointer.
  // CHECK: [[ERR_TEMP1:%.*]] = alloc_stack $@sil_unmanaged Optional<NSError>
  // CHECK: [[T0:%.*]] = load [[ERR_TEMP0:%.*]]#1
  // CHECK: [[T1:%.*]] = ref_to_unmanaged [[T0]]
  // CHECK: store [[T1]] to [[ERR_TEMP1]]#1
  // CHECK: address_to_pointer [[ERR_TEMP1]]#1

  //   Call the method.
  // CHECK: [[RESULT:%.*]] = apply [[METHOD]]({{.*}}, [[OBJC_SELF]])

  //   Writeback to the first temporary.
  // CHECK: [[T0:%.*]] = load [[ERR_TEMP1]]#1
  // CHECK: [[T1:%.*]] = unmanaged_to_ref [[T0]]
  // CHECK: retain_value [[T1]]
  // CHECK: assign [[T1]] to [[ERR_TEMP0]]#1

  //   Pull out the boolean value and compare it to zero.
  // CHECK: [[T0:%.*]] = struct_extract [[RESULT]]
  // CHECK: [[T1:%.*]] = integer_literal $[[PRIM:Builtin.Int[0-9]+]], 0
  // CHECK: [[T2:%.*]] = builtin "cmp_eq"([[T0]] : $[[PRIM]], [[T1]] : $[[PRIM]])
  // CHECK: cond_br [[T2]], [[ERROR_BB:bb[0-9]+]], [[NORMAL_BB:bb[0-9]+]]
  try ErrorProne.fail()

  //   Normal path: fall out and return.
  // CHECK: [[NORMAL_BB]]:
  // CHECK: return
  
  //   Error path: fall out and rethrow.
  // CHECK: [[ERROR_BB]]:
  // CHECK: [[T0:%.*]] = load [[ERR_TEMP0]]#1
  // CHECK: [[T1:%.*]] = function_ref @swift_convertNSErrorToErrorType : $@convention(thin) (@owned Optional<NSError>) -> @owned ErrorType
  // CHECK: [[T2:%.*]] = apply [[T1]]([[T0]])
  // CHECK: throw [[T2]] : $ErrorType
}

extension NSObject {
  @objc func abort() throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
// CHECK-LABEL: sil hidden @_TToFE14foreign_errorsCSo8NSObject5abortfS0_FzT_T_ : $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, NSObject) -> Bool
// CHECK: [[T0:%.*]] = function_ref @_TFE14foreign_errorsCSo8NSObject5abortfS0_FzT_T_ : $@convention(method) (@guaranteed NSObject) -> @error ErrorType
// CHECK: try_apply [[T0]](
// CHECK: bb1(
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T1:%.*]] = struct $Bool ([[T0]] : $Builtin.Int1)
// CHECK:   br bb3([[T1]] : $Bool)
// CHECK: bb2([[ERR:%.*]] : $ErrorType):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorTypeToNSError : $@convention(thin) (@owned ErrorType) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.Some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVSs33AutoreleasingUnsafeMutablePointers6memoryq_ :
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [[TEMP]]#1
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]]#1, %0)
// CHECK:   dealloc_stack [[TEMP]]#0
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int1, 0
// CHECK:   [[T1:%.*]] = struct $Bool ([[T0]] : $Builtin.Int1)
// CHECK:   br bb3([[T1]] : $Bool)
// CHECK: bb3([[T0:%.*]] : $Bool):
// CHECK:   return [[T0]] : $Bool

  @objc func badDescription() throws -> String {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
// CHECK-LABEL: sil hidden @_TToFE14foreign_errorsCSo8NSObject14badDescriptionfS0_FzT_SS : $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, NSObject) -> @autoreleased Optional<NSString>
// CHECK: [[T0:%.*]] = function_ref @_TFE14foreign_errorsCSo8NSObject14badDescriptionfS0_FzT_SS : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error ErrorType)
// CHECK: try_apply [[T0]](
// CHECK: bb1([[RESULT:%.*]] : $String):
// CHECK:   [[T0:%.*]] = function_ref @swift_StringToNSString : $@convention(thin) (@owned String) -> @owned NSString
// CHECK:   [[T1:%.*]] = apply [[T0]]([[RESULT]])
// CHECK:   [[T2:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[T1]] : $NSString
// CHECK:   br bb3([[T2]] : $Optional<NSString>)
// CHECK: bb2([[ERR:%.*]] : $ErrorType):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorTypeToNSError : $@convention(thin) (@owned ErrorType) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.Some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVSs33AutoreleasingUnsafeMutablePointers6memoryq_ :
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [[TEMP]]#1
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]]#1, %0)
// CHECK:   dealloc_stack [[TEMP]]#0
// CHECK:   [[T0:%.*]] = enum $Optional<NSString>, #Optional.None!enumelt
// CHECK:   br bb3([[T0]] : $Optional<NSString>)
// CHECK: bb3([[T0:%.*]] : $Optional<NSString>):
// CHECK:   autorelease_return [[T0]] : $Optional<NSString>

// CHECK-LABEL: sil hidden @_TToFE14foreign_errorsCSo8NSObject7takeIntfS0_FzSiT_ : $@convention(objc_method) (Int, AutoreleasingUnsafeMutablePointer<Optional<NSError>>, NSObject) -> Bool
// CHECK: bb0([[I:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>, [[SELF:%[0-9]+]] : $NSObject)
  @objc func takeInt(i: Int) throws { }

// CHECK-LABEL: sil hidden @_TToFE14foreign_errorsCSo8NSObject10takeDoublefS0_FzTSd3intSi7closureFSiSi_T_ : $@convention(objc_method) (Double, Int, AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @convention(block) (Int) -> Int, NSObject) -> Bool
// CHECK: bb0([[D:%[0-9]+]] : $Double, [[INT:%[0-9]+]] : $Int, [[ERROR:%[0-9]+]] : $AutoreleasingUnsafeMutablePointer<Optional<NSError>>, [[CLOSURE:%[0-9]+]] : $@convention(block) (Int) -> Int, [[SELF:%[0-9]+]] : $NSObject):
  @objc func takeDouble(d: Double, int: Int, closure: (Int) -> Int) throws {
    throw NSError(domain: "", code: 1, userInfo: [:])
  }
}

let fn = ErrorProne.fail
// CHECK-LABEL: sil shared @_TTOZFCSo10ErrorProne4failFMS_FzT_T_ : $@convention(thin) (@thick ErrorProne.Type) -> @owned @callee_owned () -> @error ErrorType
// CHECK:      [[T0:%.*]] = function_ref @_TTOZFCSo10ErrorProne4failfMS_FzT_T_ : $@convention(thin) (@thick ErrorProne.Type) -> @error ErrorType
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[T0]](%0)
// CHECK-NEXT: return [[T1]]

// CHECK-LABEL: sil shared @_TTOZFCSo10ErrorProne4failfMS_FzT_T_ : $@convention(thin) (@thick ErrorProne.Type) -> @error ErrorType {
// CHECK:      [[SELF:%.*]] = thick_to_objc_metatype %0 : $@thick ErrorProne.Type to $@objc_metatype ErrorProne.Type
// CHECK:      [[METHOD:%.*]] = class_method [volatile] [[T0]] : $@objc_metatype ErrorProne.Type, #ErrorProne.fail!1.foreign : ErrorProne.Type -> () throws -> () , $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @objc_metatype ErrorProne.Type) -> Bool
// CHECK:      [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:      [[RESULT:%.*]] = apply [[METHOD]]({{%.*}}, [[SELF]])
// CHECK:      cond_br
// CHECK:      return
// CHECK:      [[T0:%.*]] = load [[TEMP]]#1
// CHECK:      [[T1:%.*]] = apply {{%.*}}([[T0]])
// CHECK:      throw [[T1]]

func testArgs() throws {
  try ErrorProne.consume(nil)
}
// CHECK: sil hidden @_TF14foreign_errors8testArgsFzT_T_ : $@convention(thin) () -> @error ErrorType
// CHECK:   class_method [volatile] %0 : $@thick ErrorProne.Type, #ErrorProne.consume!1.foreign : ErrorProne.Type -> (AnyObject!) throws -> () , $@convention(objc_method) (ImplicitlyUnwrappedOptional<AnyObject>, AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @objc_metatype ErrorProne.Type) -> Bool

func testBridgedResult() throws {
  let array = try ErrorProne.collectionWithCount(0)
}
// CHECK: sil hidden @_TF14foreign_errors17testBridgedResultFzT_T_ : $@convention(thin) () -> @error ErrorType {
// CHECK:   class_method [volatile] %0 : $@thick ErrorProne.Type, #ErrorProne.collectionWithCount!1.foreign : ErrorProne.Type -> (Int) throws -> [AnyObject] , $@convention(objc_method) (Int, AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @objc_metatype ErrorProne.Type) -> @autoreleased Optional<NSArray>

// rdar://20861374
// Clear out the self box before delegating.
class VeryErrorProne : ErrorProne {
  init(withTwo two: AnyObject?) throws {
    try super.init(one: two)
  }
}
// CHECK:    sil hidden @_TFC14foreign_errors14VeryErrorPronecfMS0_FzT7withTwoGSqPSs9AnyObject___S0_
// CHECK:      [[BOX:%.*]] = alloc_box $VeryErrorProne
// CHECK:      [[MARKED_BOX:%.*]] = mark_uninitialized [derivedself] [[BOX]]#1
// CHECK:      [[T0:%.*]] = load [[MARKED_BOX]]
// CHECK-NEXT: [[T1:%.*]] = upcast [[T0]] : $VeryErrorProne to $ErrorProne
// CHECK-NEXT: [[T2:%.*]] = super_method [volatile] [[T0]] : $VeryErrorProne, #ErrorProne.init!initializer.1.foreign : ErrorProne.Type -> (one: AnyObject?) throws -> ErrorProne , $@convention(objc_method) (Optional<AnyObject>, AutoreleasingUnsafeMutablePointer<Optional<NSError>>, @owned ErrorProne) -> @owned Optional<ErrorProne>
// CHECK:      {{$}}
// CHECK-NOT:  [[BOX]]{{^[0-9]}}
// CHECK-NOT:  [[MARKED_BOX]]{{^[0-9]}}
// CHECK:      [[T3:%.*]] = null_class $VeryErrorProne
// CHECK-NEXT: store [[T3]] to [[MARKED_BOX]]
// CHECK-NEXT: apply [[T2]](%0, {{%.*}}, [[T1]])

// rdar://21051021
// CHECK: sil hidden @_TF14foreign_errors12testProtocolFzPSo18ErrorProneProtocol_T_
func testProtocol(p: ErrorProneProtocol) throws {
  // CHECK:   [[T0:%.*]] = open_existential_ref %0 : $ErrorProneProtocol to $[[OPENED:@opened(.*) ErrorProneProtocol]]
  // CHECK:   [[T1:%.*]] = witness_method [volatile] $[[OPENED]], #ErrorProneProtocol.obliterate!1.foreign, [[T0]] : $[[OPENED]] :
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, τ_0_0) -> Bool
  try p.obliterate()

  // CHECK:   [[T0:%.*]] = open_existential_ref %0 : $ErrorProneProtocol to $[[OPENED:@opened(.*) ErrorProneProtocol]]
  // CHECK:   [[T1:%.*]] = witness_method [volatile] $[[OPENED]], #ErrorProneProtocol.invigorate!1.foreign, [[T0]] : $[[OPENED]] :
  // CHECK:   apply [[T1]]<[[OPENED]]>({{%.*}}, {{%.*}}, [[T0]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : ErrorProneProtocol> (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, ImplicitlyUnwrappedOptional<@convention(block) () -> ()>, τ_0_0) -> Bool
  try p.invigorate(callback: {})
}

