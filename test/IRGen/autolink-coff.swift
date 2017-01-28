// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-IR
// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -S -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-ASM

// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-IR
// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -S -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-ASM

// REQUIRES: CODEGENERATOR=ARM

#if MAIN_MODULE
import module
#endif

// CHECK-MSVC-IR: !{{[0-9]+}} = !{i32 {{[0-9]+}}, !"Linker Options", [[NODE:![0-9]+]]}
// CHECK-MSVC-IR: [[NODE]] = !{[[LIST:![0-9]+]]}
// CHECK-MSVC-IR: [[LIST]] = !{!"/DEFAULTLIB:module.lib"}

// CHECK-MSVC-ASM: .section .drectve
// CHECK-MSVC-ASM: .ascii " /DEFAULTLIB:module.lib"

