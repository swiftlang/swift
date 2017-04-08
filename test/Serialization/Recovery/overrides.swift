// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -enable-experimental-deserialization-recovery | %FileCheck -check-prefix CHECK-RECOVERY %s

// REQUIRES: objc_interop

import Overrides

public class Sub: Base {
  public override func method() {}
}

// CHECK-LABEL: class Sub : Base {
// CHECK-NEXT: func method()
// CHECK-NEXT: init()
// CHECK-NEXT: {{^}$}}

// CHECK-RECOVERY-LABEL: class Sub : Base {
// CHECK-RECOVERY-NEXT: init()
// CHECK-RECOVERY-NEXT: {{^}$}}