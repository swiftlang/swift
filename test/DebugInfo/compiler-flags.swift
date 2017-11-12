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


// Check that we tune the debug output for LLDB.
// Adapted from llvm/test/DebugInfo/X86/debugger-tune.ll
// RUN: %target-swiftc_driver -emit-object -g %s -o %t
// RUN: llvm-readobj -sections %t | %FileCheck --check-prefix=CHECK-LLDB %s

// CHECK-LLDB-NOT: debug_pubnames
// CHECK-LLDB:     apple_names
// CHECK-LLDB-NOT: debug_pubnames
