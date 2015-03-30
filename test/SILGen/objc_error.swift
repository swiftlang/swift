// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF10objc_error25NSError_ErrorType_erasureFCSo7NSErrorPSs10_ErrorType_
// CHECK:         [[ERROR_TYPE:%.*]] = init_existential_ref %0 : $NSError : $NSError, $_ErrorType
// CHECK:         return [[ERROR_TYPE]]
func NSError_ErrorType_erasure(x: NSError) -> _ErrorType {
  return x
}
