// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -swift-version 3 %s

// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 3 -disable-deserialization-recovery -Xcc -DBAD - 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-3 %s
// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 4.2 -disable-deserialization-recovery -Xcc -DBAD - 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-4 %s

// REQUIRES: objc_interop

import Overrides

public class Sub: Base {
  public override func disappearingMethod() {}
}

// CHECK-CRASH: error: fatal error encountered while reading from module 'Lib'; please file a bug report with your project and the crash log
// CHECK-CRASH-3-NOT: note
// CHECK-CRASH-4: note: compiling as Swift 4.2, with 'Lib' built as Swift 3.4
// CHECK-CRASH-LABEL: *** DESERIALIZATION FAILURE (please include this section in any bug report) ***
// CHECK-CRASH: could not find 'disappearingMethod()' in parent class
// CHECK-CRASH: While loading members for 'Sub' in module 'Lib'
