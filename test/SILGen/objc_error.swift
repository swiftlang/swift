// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF10objc_error20NSErrorError_erasureFCSo7NSErrorPs5Error_
// CHECK:         [[ERROR_TYPE:%.*]] = init_existential_ref %0 : $NSError : $NSError, $Error
// CHECK:         return [[ERROR_TYPE]]
func NSErrorError_erasure(_ x: NSError) -> Error {
  return x
}

// CHECK-LABEL: sil hidden @_TF10objc_error30NSErrorError_archetype_erasure
// CHECK:         [[T0:%.*]] = upcast %0 : $T to $NSError
// CHECK:         [[ERROR_TYPE:%.*]] = init_existential_ref [[T0]] : $NSError : $NSError, $Error
// CHECK:         return [[ERROR_TYPE]]
func NSErrorError_archetype_erasure<T : NSError>(_ t: T) -> Error {
  return t
}

// Test patterns that are non-trivial, but irrefutable.  SILGen shouldn't crash
// on these.
func test_doesnt_throw() {
  do {
    throw NSError(domain: "", code: 1, userInfo: [:])
  } catch is Error {  // expected-warning {{'is' test is always true}}
  }

  do {
    throw NSError(domain: "", code: 1, userInfo: [:])
  } catch let e as NSError {  // ok.
    _ = e
  }
}

class ErrorClass: Error {
  let _domain = ""
  let _code = 0
}

// Class-to-NSError casts must be done as indirect casts since they require
// a representation change, and checked_cast_br currently doesn't allow that.

// CHECK-LABEL: sil hidden @_TF10objc_error20test_cast_to_nserrorFT_T_
func test_cast_to_nserror() {
  let e = ErrorClass()

  // CHECK: function_ref @swift_bridgeErrorToNSError
  let nsCoerced = e as Error as NSError

  // CHECK: unconditional_checked_cast_addr {{.*}} AnyObject in {{%.*}} : $*AnyObject to NSError in {{%.*}} : $*NSError
  let nsForcedCast = (e as AnyObject) as! NSError

  // CHECK: checked_cast_addr_br {{.*}} Error in {{%.*}} : $*Error to NSError in {{%.*}} : $*NSError, bb3, bb4
  do {
    throw e
  } catch _ as NSError {
    
  }
}

// A class-constrained archetype may be NSError, so we can't use scalar casts
// in that case either.
// CHECK-LABEL: sil hidden @_TF10objc_error28test_cast_to_class_archetype
func test_cast_to_class_archetype<T: AnyObject>(_: T) {
  // CHECK: unconditional_checked_cast_addr {{.*}} ErrorClass in {{%.*}} : $*ErrorClass to T in {{.*}} : $*T
  let e = ErrorClass()
  let forcedCast = e as! T
}

// CHECK-LABEL: sil hidden @_TF10objc_error15testAcceptError
func testAcceptError(error: Error) {
  // CHECK-NOT: return
  // CHECK: function_ref @swift_convertErrorToNSError
  acceptError(error)
}

// CHECK-LABEL: sil hidden @_TF10objc_error16testProduceError
func testProduceError() -> Error {
  // CHECK: function_ref @produceError : $@convention(c) () -> @autoreleased NSError
  // CHECK: init_existential_ref {{.*}} : $NSError : $NSError, $Error
  return produceError()
}

// CHECK-LABEL: sil hidden @_TF10objc_error24testProduceOptionalError
func testProduceOptionalError() -> Error? {
  // CHECK: function_ref @produceOptionalError
  // CHECK: init_existential_ref {{.*}} : $NSError : $NSError, $Error
  return produceOptionalError();
}

class MyNSError : NSError {
  override init() {
    super.init(domain: "MyNSError", code: 0, userInfo: [:])
  }
}

// CHECK-LABEL: sil hidden @_TF10objc_error14eraseMyNSError
// CHECK-NOT: return
// CHECK: init_existential_ref
func eraseMyNSError() -> Error {
  return MyNSError()
}

// CHECK-LABEL: sil hidden @_TF10objc_error25eraseFictionalServerErrorFT_Ps5Error_
func eraseFictionalServerError() -> Error {
  // CHECK-NOT: return
  // CHECK: [[NSERROR:%[0-9]+]] = struct_extract {{.*}} : $FictionalServerError, #FictionalServerError._nsError
  // CHECK: [[ERROR:%[0-9]+]] = init_existential_ref [[NSERROR]]
  // CHECK: return [[ERROR]]
  return FictionalServerError(.meltedDown)
}

// SR-1562
extension Error {
  // CHECK-LABEL: sil hidden @_TFE10objc_errorPs5Error16convertToNSErrorfT_CSo7NSError
  // CHECK: bb0([[SELF:%[0-9]+]] : $*Self)
	func convertToNSError() -> NSError {
    // CHECK: [[COPY:%.*]] = alloc_stack $Self
    // CHECK: copy_addr [[SELF]] to [initialization] [[COPY]]
    // CHECK: [[COPY2:%.*]] = alloc_stack $Self
    // CHECK: copy_addr [[COPY]] to [initialization] [[COPY2]]
    // CHECK: [[GET_EMBEDDED_FN:%[0-9]+]] = function_ref @swift_stdlib_getErrorEmbeddedNSError
    // CHECK: [[EMBEDDED_RESULT_OPT:%[0-9]+]] = apply [[GET_EMBEDDED_FN]]<Self>([[COPY2]])
    // CHECK: switch_enum [[EMBEDDED_RESULT_OPT]] : $Optional<AnyObject>,
    // CHECK-SAME: case #Optional.some!enumelt.1: [[SUCCESS:bb[0-9]+]],
    // CHECK-SAME: case #Optional.none!enumelt: [[FAILURE:bb[0-9]+]]

    // CHECK: [[SUCCESS]]([[EMBEDDED_RESULT:%.*]] : $AnyObject):
    // CHECK: [[EMBEDDED_NSERROR:%[0-9]+]] = unchecked_ref_cast [[EMBEDDED_RESULT]] : $AnyObject to $NSError
    // CHECK: [[ERROR:%[0-9]+]] = init_existential_ref [[EMBEDDED_NSERROR]] : $NSError : $NSError, $Error
    // CHECK: destroy_addr [[COPY]] : $*Self
    // CHECK: br [[CONTINUATION:bb[0-9]+]]([[ERROR]] : $Error)

    // CHECK: [[FAILURE]]:
    // CHECK: [[ERROR_BOX:%[0-9]+]] = alloc_existential_box $Error, $Self
    // CHECK: [[ERROR_PROJECTED:%[0-9]+]] = project_existential_box $Self in [[ERROR_BOX]] : $Error
    // CHECK: copy_addr [take] [[COPY]] to [initialization] [[ERROR_PROJECTED]] : $*Self
    // CHECK: br [[CONTINUATION]]([[ERROR_BOX]] : $Error)

    // CHECK: [[CONTINUATION]]([[ERROR_ARG:%[0-9]+]] : $Error):
		return self as NSError
	}
}
