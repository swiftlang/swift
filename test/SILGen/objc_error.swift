
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -module-name objc_error -emit-silgen -enable-sil-ownership %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @$S10objc_error20NSErrorError_erasureys0D0_pSo0C0CF : $@convention(thin) (@guaranteed NSError) -> @owned Error {
// CHECK:         bb0([[ERROR:%.*]] : @guaranteed $NSError):
// CHECK:           [[ERROR_COPY:%.*]] = copy_value [[ERROR]]
// CHECK:           [[ERROR_TYPE:%.*]] = init_existential_ref [[ERROR_COPY]] : $NSError : $NSError, $Error
// CHECK-NOT:           destroy_value [[ERROR]]
// CHECK:           return [[ERROR_TYPE]]
// CHECK:       } // end sil function '$S10objc_error20NSErrorError_erasureys0D0_pSo0C0CF'
func NSErrorError_erasure(_ x: NSError) -> Error {
  return x
}

// CHECK-LABEL: sil hidden @$S10objc_error30NSErrorError_archetype_erasureys0D0_pxSo0C0CRbzlF : $@convention(thin) <T where T : NSError> (@guaranteed T) -> @owned Error {
// CHECK:         bb0([[ERROR:%.*]] : @guaranteed $T):
// CHECK:           [[ERROR_COPY:%.*]] = copy_value [[ERROR]]
// CHECK:           [[T0:%.*]] = upcast [[ERROR_COPY]] : $T to $NSError
// CHECK:           [[ERROR_TYPE:%.*]] = init_existential_ref [[T0]] : $NSError : $NSError, $Error
// CHECK-NOT:           destroy_value [[ERROR]]
// CHECK:           return [[ERROR_TYPE]]
// CHECK: } // end sil function '$S10objc_error30NSErrorError_archetype_erasureys0D0_pxSo0C0CRbzlF'
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

// CHECK-LABEL: sil hidden @$S10objc_error20test_cast_to_nserroryyF
func test_cast_to_nserror() {
  let e = ErrorClass()

  // CHECK: function_ref @$S10Foundation22_convertErrorToNSErrorySo0E0Cs0C0_pF
  let nsCoerced = e as Error as NSError

  // CHECK: unconditional_checked_cast_addr AnyObject in {{%.*}} : $*AnyObject to NSError in {{%.*}} : $*NSError
  let nsForcedCast = (e as AnyObject) as! NSError

  // CHECK: checked_cast_addr_br {{.*}} Error in {{%.*}} : $*Error to NSError in {{%.*}} : $*NSError, bb3, bb4
  do {
    throw e
  } catch _ as NSError {
    
  }
}

// A class-constrained archetype may be NSError, so we can't use scalar casts
// in that case either.
// CHECK-LABEL: sil hidden @$S10objc_error28test_cast_to_class_archetype{{[_0-9a-zA-Z]*}}F
func test_cast_to_class_archetype<T: AnyObject>(_: T) {
  // CHECK: unconditional_checked_cast_addr ErrorClass in {{%.*}} : $*ErrorClass to T in {{.*}} : $*T
  let e = ErrorClass()
  let forcedCast = e as! T
}

// CHECK-LABEL: sil hidden @$S10objc_error15testAcceptError{{[_0-9a-zA-Z]*}}F
func testAcceptError(error: Error) {
  // CHECK-NOT: return
  // CHECK: function_ref @$S10Foundation22_convertErrorToNSErrorySo0E0Cs0C0_pF
  acceptError(error)
}

// CHECK-LABEL: sil hidden @$S10objc_error16testProduceError{{[_0-9a-zA-Z]*}}F
func testProduceError() -> Error {
  // CHECK: function_ref @produceError : $@convention(c) () -> @autoreleased NSError
  // CHECK: init_existential_ref {{.*}} : $NSError : $NSError, $Error
  return produceError()
}

// CHECK-LABEL: sil hidden @$S10objc_error24testProduceOptionalError{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @$S10objc_error14eraseMyNSError{{[_0-9a-zA-Z]*}}F : $@convention(thin) () -> @owned Error {
// CHECK: bb0:
// CHECK:   [[NSERROR_SUBCLASS:%.*]] = apply {{.*}}({{.*}}) : $@convention(method) (@thick MyNSError.Type) -> @owned MyNSError
// CHECK:   [[UPCAST:%.*]] = upcast [[NSERROR_SUBCLASS]] : $MyNSError to $NSError
// CHECK:   [[EXISTENTIAL_REF:%.*]] = init_existential_ref [[UPCAST]]
// CHECK:   [[BORROWED_EXISTENTIAL_REF:%.*]] = begin_borrow [[EXISTENTIAL_REF]]
// CHECK:   [[COPY_BORROWED_EXISTENTIAL_REF:%.*]] = copy_value [[BORROWED_EXISTENTIAL_REF]]
// CHECK:   end_borrow [[BORROWED_EXISTENTIAL_REF]] from [[EXISTENTIAL_REF]]
// CHECK:   destroy_value [[EXISTENTIAL_REF]]
// CHECK:   return [[COPY_BORROWED_EXISTENTIAL_REF]]
// CHECK: } // end sil function '$S10objc_error14eraseMyNSError{{[_0-9a-zA-Z]*}}F'
func eraseMyNSError() -> Error {
  let x: Error = MyNSError()
  return x
}

// CHECK-LABEL: sil hidden @$S10objc_error25eraseFictionalServerErrors0F0_pyF
func eraseFictionalServerError() -> Error {
  // CHECK-NOT: return
  // CHECK: [[NSERROR:%[0-9]+]] = struct_extract {{.*}} : $FictionalServerError, #FictionalServerError._nsError
  // CHECK: [[NSERROR_COPY:%.*]] = copy_value [[NSERROR]]
  // CHECK: [[ERROR:%[0-9]+]] = init_existential_ref [[NSERROR_COPY]]
  // CHECK: return [[ERROR]]
  return FictionalServerError(.meltedDown)
}
// CHECK: } // end sil function '$S10objc_error25eraseFictionalServerErrors0F0_pyF'

// SR-1562
extension Error {
  // CHECK-LABEL: sil hidden @$Ss5ErrorP10objc_errorE16convertToNSErrorSo0F0CyF
  // CHECK: bb0([[SELF:%[0-9]+]] : @trivial $*Self)
	func convertToNSError() -> NSError {
    // CHECK: [[COPY:%.*]] = alloc_stack $Self
    // CHECK: copy_addr [[SELF]] to [initialization] [[COPY]]
    // CHECK: [[COPY2:%.*]] = alloc_stack $Self
    // CHECK: copy_addr [[COPY]] to [initialization] [[COPY2]]
    // CHECK: [[GET_EMBEDDED_FN:%[0-9]+]] = function_ref @$Ss24_getErrorEmbeddedNSErroryyXlSgxs0B0RzlF
    // CHECK: [[EMBEDDED_RESULT_OPT:%[0-9]+]] = apply [[GET_EMBEDDED_FN]]<Self>([[COPY2]])
    // CHECK: switch_enum [[EMBEDDED_RESULT_OPT]] : $Optional<AnyObject>,
    // CHECK-SAME: case #Optional.some!enumelt.1: [[SUCCESS:bb[0-9]+]],
    // CHECK-SAME: case #Optional.none!enumelt: [[FAILURE:bb[0-9]+]]

    // CHECK: [[SUCCESS]]([[EMBEDDED_RESULT:%.*]] : @owned $AnyObject):
    // CHECK: [[EMBEDDED_NSERROR:%[0-9]+]] = unchecked_ref_cast [[EMBEDDED_RESULT]] : $AnyObject to $NSError
    // CHECK: [[ERROR:%[0-9]+]] = init_existential_ref [[EMBEDDED_NSERROR]] : $NSError : $NSError, $Error
    // CHECK: destroy_addr [[COPY]] : $*Self
    // CHECK: br [[CONTINUATION:bb[0-9]+]]([[ERROR]] : $Error)

    // CHECK: [[FAILURE]]:
    // CHECK: [[ERROR_BOX:%[0-9]+]] = alloc_existential_box $Error, $Self
    // CHECK: [[ERROR_PROJECTED:%[0-9]+]] = project_existential_box $Self in [[ERROR_BOX]] : $Error
    // CHECK: copy_addr [take] [[COPY]] to [initialization] [[ERROR_PROJECTED]] : $*Self
    // CHECK: br [[CONTINUATION]]([[ERROR_BOX]] : $Error)

    // CHECK: [[CONTINUATION]]([[ERROR_ARG:%[0-9]+]] : @owned $Error):
		return self as NSError
	}
}

class Gizmoid : NSObject {
  // CHECK-LABEL: sil hidden [thunk] @$S10objc_error7GizmoidC3fooACyt_tKcfcTo : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, @owned Gizmoid) -> @owned Optional<Gizmoid>
  @objc init(foo: ()) throws {}
}
