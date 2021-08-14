// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -swift-version 4 %s

// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 4 -disable-deserialization-recovery -Xcc -DBAD - 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-4 %s
// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 4.2 -disable-deserialization-recovery -Xcc -DBAD - 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-4_2 %s

// REQUIRES: objc_interop

import Overrides

public class Sub: Base {
  public override func disappearingMethod() {}
}

// CHECK-CRASH: error: fatal error encountered while reading from module 'Lib'; please submit a bug report (https://swift.org/contributing/#reporting-bugs) and include the project
// CHECK-CRASH-4: Compiling with effective version 4.1.50
// CHECK-CRASH-4_2: Compiling with effective version 4.2
// CHECK-CRASH: While loading members for 'Sub' (in module 'Lib')
// CHECK-CRASH-LABEL: *** DESERIALIZATION FAILURE ***
// CHECK-CRASH: module 'Lib' with full misc version {{.*}}4.1.50
// CHECK-CRASH: could not find 'disappearingMethod()' in parent class
