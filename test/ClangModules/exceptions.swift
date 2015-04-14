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