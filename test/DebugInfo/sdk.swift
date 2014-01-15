// Check that the sdk ends up in the debug info.
// RUN: %swift %s -emit-llvm -g -o - | FileCheck %s
// RUN: %swift %s -emit-llvm -sdk="/Weird Location/SDK" -g -o - | FileCheck --check-prefix CHECK-EXPLICIT %s
// CHECK:          metadata !"Swift version {{.*}} (based on LLVM {{.*}})", i1 false, metadata !"{{.*}} -sdk={{.*}}"
// CHECK-EXPLICIT: metadata !"Swift version {{.*}} (based on LLVM {{.*}})", i1 false, metadata !"{{.*}} \22-sdk=/Weird Location/SDK\22{{.*}}"
