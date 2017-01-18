// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -target x86_64-scei-ps4 -parse-as-library -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target x86_64-scei-ps4 -parse-as-library -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-IR

#if MAIN_MODULE
import module
#endif

// CHECK-IR: !{{[0-9]+}} = !{i32 {{[0-9]+}}, !"Linker Options", [[NODE:![0-9]+]]}
// CHECK-IR: [[NODE]] = !{[[LIST:![0-9]+]]}
// CHECK-IR: [[LIST]] = !{!"\01module"}

