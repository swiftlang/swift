// RUN: %empty-directory(%t)
// RUN: %swift -target x86_64-scei-ps4 -parse-as-library -disable-legacy-type-info -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target x86_64-scei-ps4 -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-IR

#if MAIN_MODULE
import module
#endif

// CHECK-IR: !llvm.linker.options = !{[[LIST:![0-9]+]]}
// CHECK-IR: [[LIST]] = !{!"\01module"}

