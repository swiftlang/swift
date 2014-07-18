// RUN: %swift -Xllvm -help 2>&1 | FileCheck %s --check-prefix=CHECK-HELP
// CHECK-HELP: -fatal-assembler-warnings

// RUN: %swift -Xllvm -fatal-assembler-warnings -emit-sil %s 2>&1 | FileCheck %s -check-prefix=CHECK-SIL
// CHECK-SIL: top_level_code
