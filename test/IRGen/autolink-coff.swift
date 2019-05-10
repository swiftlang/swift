// RUN: %empty-directory(%t)

// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -disable-legacy-type-info -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-IR
// RUN: %swift -target thumbv7--windows-itanium -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -S -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-ASM

// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -disable-legacy-type-info -parse-stdlib -emit-module-path %t/module.swiftmodule -module-name module -module-link-name module %s
// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -emit-ir -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-IR
// RUN: %swift -target thumbv7--windows-msvc -parse-as-library -disable-legacy-type-info -parse-stdlib -module-name autolink -I %t -D MAIN_MODULE -S -o - %s | %FileCheck %s -check-prefix CHECK-MSVC-ASM

// REQUIRES: CODEGENERATOR=ARM

#if MAIN_MODULE
import module
#endif

// CHECK-MSVC-IR: !llvm.linker.options = !{[[LIST:![0-9]+]]}
// CHECK-MSVC-IR: [[LIST]] = !{!"/DEFAULTLIB:module.lib"}

// CHECK-MSVC-ASM: .section .drectve
// CHECK-MSVC-ASM: .ascii " /DEFAULTLIB:module.lib"

