// Check that the sdk and resource dirs end up in the debug info if we pass the
// frontend flag. This tests the general functionality; we test the macosx
// specific toolchain logic in compiler-flags-macosx.swift.

// RUN: %target-swiftc_driver %s -emit-ir -debug-info-store-invocation -g -o - | %FileCheck %s
// RUN: %target-swiftc_driver %s -emit-ir -debug-info-store-invocation -sdk "/Weird Location/SDK" -g -o - | %FileCheck --check-prefix CHECK-EXPLICIT %s
// CHECK:          !DICompileUnit({{.*}}producer: "{{[^"]*Swift version [^"]+}}"
// CHECK-SAME:                    flags: "
// CHECK-NOT:                     "
// CHECK-SAME:                    -resource-dir 
// CHECK-EXPLICIT: !DICompileUnit({{.*}}producer: "{{[^"]*Swift version [^"]+}}"
// CHECK-EXPLICIT-SAME:           flags: "
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -sdk \22/Weird Location/SDK\22
// CHECK-EXPLICIT-NOT:            "
// CHECK-EXPLICIT-SAME:           -resource-dir 

// Check that we don't write temporary file names in the debug info
// RUN: env TMP=abc/def TMPDIR=abc/def %target-swift-frontend %s -I abc/def/xyz -g -emit-ir -debug-info-store-invocation -o - | %FileCheck --check-prefix CHECK-TEMP %s
// RUN: env TMP=%t TMPDIR=%t %target-swift-frontend %s -I %t/xyz -g -emit-ir -debug-info-store-invocation -o - | %FileCheck --check-prefix CHECK-TEMP %s
// CHECK-TEMP: !DICompileUnit({{.*}} flags: "{{.*}} -I {{(\\22)?}}<temporary-file>

