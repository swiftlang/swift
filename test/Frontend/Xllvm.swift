// RUN: %swift -repl -Xllvm -help 2>&1 | %FileCheck %s --check-prefix=CHECK-HELP
// CHECK-HELP: -version

// RUN: %swift -Xllvm -version -emit-sil %s 2>&1 | %FileCheck %s -check-prefix=CHECK-SIL
// CHECK-SIL: LLVM (http://llvm.org/)
