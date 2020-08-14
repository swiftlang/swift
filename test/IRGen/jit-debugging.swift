// %empty-directory(%t)
// RUN: not %target-swift-frontend -use-jit -dump-jit invalid -interpret %s 2>&1 | %FileCheck -check-prefix CHECK-INVALID %s
// CHECK-INVALID: error: invalid value 'invalid' in 'dump-jit'

// RUN: %empty-directory(%t)
// RUN: cd %t && %target-swift-frontend -use-jit -dump-jit llvm-ir -interpret %s
// RUN: %FileCheck -check-prefix CHECK-LLIR %s < %t/main.ll
// CHECK-LLIR: ; ModuleID = 'main'

// RUN: %empty-directory(%t)
// RUN: cd %t && %target-swift-frontend -use-jit -dump-jit object -interpret %s
// RUN: %llvm-nm --defined-only --extern-only %t/main-jitted-objectbuffer.o | %FileCheck -check-prefix CHECK-OBJ %s
// CHECK-OBJ: T {{_?}}main

// REQUIRES: rdar66644853

let zero = 0
