// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF10objc_error24NSErrorErrorType_erasureFCSo7NSErrorPSs9ErrorType_
// CHECK:         [[ERROR_TYPE:%.*]] = init_existential_ref %0 : $NSError : $NSError, $ErrorType
// CHECK:         return [[ERROR_TYPE]]
func NSErrorErrorType_erasure(x: NSError) -> ErrorType {
  return x
}

// Test patterns that are non-trivial, but irrefutable.  SILGen shouldn't crash
// on these.
func test_doesnt_throw() {
  do {
    throw NSError(domain: "", code: 1, userInfo: [:])
  } catch is ErrorType {  // expected-warning {{'is' test is always true}}
  }

  do {
    throw NSError(domain: "", code: 1, userInfo: [:])
  } catch let e as NSError {  // ok.
    _ = e
  }
}

class ErrorClass: ErrorType {
  let _domain = ""
  let _code = 0
}

// Class-to-NSError casts must be done as indirect casts since they require
// a representation change, and checked_cast_br currently doesn't allow that.

// CHECK-LABEL: sil hidden @_TF10objc_error20test_cast_to_nserrorFT_T_
func test_cast_to_nserror() {
  let e = ErrorClass()

  // CHECK: function_ref @swift_bridgeErrorTypeToNSError
  let nsCoerced = e as NSError

  // CHECK: unconditional_checked_cast_addr {{.*}} AnyObject in {{%.*}} : $*AnyObject to NSError in {{%.*}} : $*NSError
  let nsForcedCast = (e as AnyObject) as! NSError

  // CHECK: checked_cast_addr_br {{.*}} ErrorType in {{%.*}} : $*ErrorType to NSError in {{%.*}} : $*NSError, bb3, bb4
  do {
    throw e
  } catch _ as NSError {
    
  }
}

// A class-constrained archetype may be NSError, so we can't use scalar casts
// in that case either.
// CHECK-LABEL: sil hidden @_TF10objc_error28test_cast_to_class_archetypeuRq_Ss9AnyObject_Fq_T_
func test_cast_to_class_archetype<T: AnyObject>(_: T) {
  // CHECK: unconditional_checked_cast_addr {{.*}} ErrorClass in {{%.*}} : $*ErrorClass to T in {{.*}} : $*T
  let e = ErrorClass()
  let forcedCast = e as! T
}
