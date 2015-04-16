// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -import-error-handling -emit-silgen -parse-as-library %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation
import exceptions

// CHECK: sil hidden @_TF10exceptions5test0FzT_T_ : $@convention(thin) () -> @error _ErrorType
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
  // CHECK: [[T1:%.*]] = function_ref @swift_convertNSErrorToErrorType : $@convention(thin) (@owned Optional<NSError>) -> @owned _ErrorType
  // CHECK: [[T2:%.*]] = apply [[T1]]([[T0]])
  // CHECK: throw [[T2]] : $_ErrorType
}

extension NSObject {
  @objc func abort() throws {
    throw NSError()
  }
// CHECK-LABEL: sil hidden @_TToFE10exceptionsCSo8NSObject5abortfS0_FzT_T_ : $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, NSObject) -> Bool
// CHECK: [[T0:%.*]] = function_ref @_TFE10exceptionsCSo8NSObject5abortfS0_FzT_T_ : $@convention(method) (@guaranteed NSObject) -> @error _ErrorType
// CHECK: try_apply [[T0]](
// CHECK: bb1(
// CHECK:   [[T0:%.*]] = integer_literal $Builtin.Int1, -1
// CHECK:   [[T1:%.*]] = struct $Bool ([[T0]] : $Builtin.Int1)
// CHECK:   br bb3([[T1]] : $Bool)
// CHECK: bb2([[ERR:%.*]] : $_ErrorType):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorTypeToNSError : $@convention(thin) (@owned _ErrorType) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.Some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVSs33AutoreleasingUnsafeMutablePointers6memoryQ_ :
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
    throw NSError()
  }
// CHECK-LABEL: sil hidden @_TToFE10exceptionsCSo8NSObject14badDescriptionfS0_FzT_SS : $@convention(objc_method) (AutoreleasingUnsafeMutablePointer<Optional<NSError>>, NSObject) -> @autoreleased Optional<NSString>
// CHECK: [[T0:%.*]] = function_ref @_TFE10exceptionsCSo8NSObject14badDescriptionfS0_FzT_SS : $@convention(method) (@guaranteed NSObject) -> (@owned String, @error _ErrorType)
// CHECK: try_apply [[T0]](
// CHECK: bb1([[RESULT:%.*]] : $String):
// CHECK:   [[T0:%.*]] = function_ref @swift_StringToNSString : $@convention(thin) (@owned String) -> @owned NSString
// CHECK:   [[T1:%.*]] = apply [[T0]]([[RESULT]])
// CHECK:   [[T2:%.*]] = enum $Optional<NSString>, #Optional.Some!enumelt.1, [[T1]] : $NSString
// CHECK:   br bb3([[T2]] : $Optional<NSString>)
// CHECK: bb2([[ERR:%.*]] : $_ErrorType):
// CHECK:   [[T0:%.*]] = function_ref @swift_convertErrorTypeToNSError : $@convention(thin) (@owned _ErrorType) -> @owned NSError
// CHECK:   [[T1:%.*]] = apply [[T0]]([[ERR]])
// CHECK:   [[OBJCERR:%.*]] = enum $Optional<NSError>, #Optional.Some!enumelt.1, [[T1]] : $NSError
// CHECK:   [[SETTER:%.*]] = function_ref @_TFVSs33AutoreleasingUnsafeMutablePointers6memoryQ_ :
// CHECK:   [[TEMP:%.*]] = alloc_stack $Optional<NSError>
// CHECK:   store [[OBJCERR]] to [[TEMP]]#1
// CHECK:   apply [[SETTER]]<Optional<NSError>>([[TEMP]]#1, %0)
// CHECK:   dealloc_stack [[TEMP]]#0
// CHECK:   [[T0:%.*]] = enum $Optional<NSString>, #Optional.None!enumelt
// CHECK:   br bb3([[T0]] : $Optional<NSString>)
// CHECK: bb3([[T0:%.*]] : $Optional<NSString>):
// CHECK:   autorelease_return [[T0]] : $Optional<NSString>
}
