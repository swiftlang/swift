// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -swift-version 4 %s

// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 4 -disable-deserialization-recovery -Xcc -DBAD - -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-4 %s
// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -swift-version 4.2 -disable-deserialization-recovery -Xcc -DBAD - -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix CHECK-CRASH -check-prefix CHECK-CRASH-4_2 %s
// RUN: echo 'import Lib; _ = Sub.disappearingMethod' | not --crash %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -debugger-support -swift-version 4 -disable-deserialization-recovery -Xcc -DBAD - -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix CHECK-CRASH-DEBUGGER %s

// REQUIRES: objc_interop

import Overrides

public class Sub: Base {
  public override func disappearingMethod() {}
}

// CHECK-CRASH: error: fatal error encountered while reading from module 'Lib'; please submit a bug report (https://swift.org/contributing/#reporting-bugs){{$}}
// CHECK-CRASH-4: Compiling with effective version 4.1.50
// CHECK-CRASH-4_2: Compiling with effective version 4.2
// CHECK-CRASH-LABEL: While loading members for 'Sub' (in module 'Lib')
// CHECK-CRASH-LABEL: *** DESERIALIZATION FAILURE ***
// CHECK-CRASH-LABEL: *** If any module named here was modified in the SDK, please delete the ***
// CHECK-CRASH-LABEL: *** new swiftmodule files from the SDK and keep only swiftinterfaces.   ***
// CHECK-CRASH: module 'Lib', builder version {{.*}}4.1.50
// CHECK-CRASH: could not find 'disappearingMethod()' in parent class
// Part of the error message:
// CHECK-CRASH-DEBUGGER: Stack dump
// Produced by fatal() because of -disable-deserialization-recovery.
// CHECK-CRASH-DEBUGGER: Stack dump
