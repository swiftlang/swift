// RUN: %target-swift-frontend -O -module-name=test -emit-sil %s | grep -v debug_value | %FileCheck %s

// REQUIRES: objc_interop

// Check if the optimizer can remove dead briding calls.

import Foundation

class Myclass : NSObject {

// CHECK-LABEL: sil private [thunk] {{.*}}@$s4test7MyclassC3fooyySSFTo
// CHECK:       bb0(%0 : $NSString, %1 : $Myclass):
// CHECK-NEXT:   tuple ()
// CHECK-NEXT:   return
  @objc func foo(_ s: String) {
  }
}

