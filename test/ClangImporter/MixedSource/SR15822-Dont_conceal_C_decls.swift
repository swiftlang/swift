// Confirm that SR-15822 is resolved.

// RUN: %empty-directory(%t)
// RUN: cd "%t"
// RUN: %target-swiftc_driver "%S/Inputs/SR15822Library.swift" -import-objc-header "%S/Inputs/SR15822.h" -emit-module -emit-library -module-name SR15822Library
// RUN: %target-swiftc_driver "%s" -o ./main -I"%t" -I"%S/Inputs" -L"%t" -lSR15822Library
// RUN: LD_LIBRARY_PATH="%t" ./main | %FileCheck %s

import SR15822ClangModule
import SR15822Library
let instance = SwiftyName()
print(instance.foo)
// CHECK: {{^foo$}}
