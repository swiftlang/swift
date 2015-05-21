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
