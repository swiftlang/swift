// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: not --crash %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD 2>&1 | %FileCheck -check-prefix CHECK-CRASH %s
// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -enable-experimental-deserialization-recovery | %FileCheck -check-prefix CHECK-RECOVERY %s

// REQUIRES: objc_interop

import Overrides

public class Sub: Base {
  public override func method() {}
}

// CHECK-LABEL: class Sub : Base {
// CHECK-NEXT: func method()
// CHECK-NEXT: {{^}$}}

// CHECK-CRASH: error: fatal error encountered while reading from module 'Lib'; please file a bug report with your project and the crash log
// CHECK-CRASH-LABEL: *** DESERIALIZATION FAILURE (please include this section in any bug report) ***
// CHECK-CRASH: could not find 'method()' in parent class
// CHECK-CRASH: While loading members for 'Sub' in module 'Lib'

// CHECK-RECOVERY-LABEL: class Sub : Base {
// CHECK-RECOVERY-NEXT: {{^}$}}