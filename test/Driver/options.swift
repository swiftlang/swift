// rdar://13723332 Crash on -emit-sil with no input files
// RUN: %swift -emit-sil 2>&1 | FileCheck %s
// CHECK: swift: no inputs specified
