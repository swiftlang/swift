// Check that the sdk and resource dirs end up in the debug info.
// RUN: %swift_driver %s -emit-ir -g -o - | FileCheck %s
// RUN: %swift_driver %s -emit-ir -sdk "/Weird Location/SDK" -g -o - | FileCheck --check-prefix CHECK-EXPLICIT %s
// CHECK:          metadata !"Swift version {{.*}}", i1 false, metadata !"{{.*}} -resource-dir {{.*}}"
// CHECK-EXPLICIT: metadata !"Swift version {{.*}}", i1 false, metadata !"{{.*}} -sdk \22/Weird Location/SDK\22{{.*}} -resource-dir {{.*}}"
