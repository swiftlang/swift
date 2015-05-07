// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: sil hidden @_TF10objc_error24NSErrorErrorType_erasureFCSo7NSErrorPSs9ErrorType_
// CHECK:         [[ERROR_TYPE:%.*]] = init_existential_ref %0 : $NSError : $NSError, $ErrorType
// CHECK:         return [[ERROR_TYPE]]
func NSErrorErrorType_erasure(x: NSError) -> ErrorType {
  return x
}
