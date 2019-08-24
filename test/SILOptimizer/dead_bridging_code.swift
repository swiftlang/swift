// RUN: %target-swift-frontend -O -module-name=test -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

// Check if the optimizer can remove dead briding calls.

import Foundation

class Myclass : NSObject {

// CHECK-LABEL: sil hidden [thunk] @$s4test7MyclassC3fooyySSFTo
// CHECK-NEXT: bb0(%0 : $NSString, %1 : $Myclass):
// CHECK-NEXT:   tuple ()
// CHECK-NEXT:   return
  @objc func foo(_ s: String) {
  }
}

