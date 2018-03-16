// Check that the sdk and resource dirs end up in the debug info.
// RUN: %target-swiftc_driver %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swiftc_driver %s -emit-ir -sdk "/Weird Location/SDK" -g -o - | %FileCheck --check-prefix CHECK-EXPLICIT %s
// CHECK:          !DICompileUnit({{.*}}producer: "{{(Apple )?Swift version [^"]+}}"
// CHECK-SAME:                    flags: "
// CHECK-NOT:                     "
// CHECK-SAME:                    -resource-dir 
// CHECK-EXPLICIT: !DICompileUnit({{.*}}producer: "{{(Apple )?Swift version [^"]+}}"
// CHECK-EXPLICIT-SAME:           flags: "
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -sdk \22/Weird Location/SDK\22
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -resource-dir 

// Check that we don't write temporary file names in the debug info
// RUN: TMPDIR=abc/def %target-swift-frontend %s -I abc/def/xyz -g -emit-ir -o - | %FileCheck --check-prefix CHECK-TEMP %s
// CHECK-TEMP: !DICompileUnit({{.*}} flags: "{{.*}} -I <temporary-file>

