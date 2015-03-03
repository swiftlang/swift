// Check that the sdk and resource dirs end up in the debug info.
// RUN: %target-swiftc_driver %s -emit-ir -g -o - | FileCheck %s
// RUN: %target-swiftc_driver %s -emit-ir -sdk "/Weird Location/SDK" -g -o - | FileCheck --check-prefix CHECK-EXPLICIT %s
// CHECK:          !MDCompileUnit({{.*}}producer: "{{(Apple )?Swift version [^"]+}}"
// CHECK-SAME:                    flags: "
// CHECK-NOT:                     "
// CHECK-SAME:                    -resource-dir 
// CHECK-EXPLICIT: !MDCompileUnit({{.*}}producer: "{{(Apple )?Swift version [^"]+}}"
// CHECK-EXPLICIT-SAME:           flags: "
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -sdk \22/Weird Location/SDK\22
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -resource-dir 
