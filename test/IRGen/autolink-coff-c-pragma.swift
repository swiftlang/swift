// Tests that a `#pragma comment(lib, ...)` in a C header imported as a module
// causes a corresponding `/DEFAULTLIB` directive to be emitted.
//
// We test that this is true also for C headers included transitively from
// another C header.

// RUN: %swift -module-name Swift -target x86_64-unknown-windows-msvc -I %S/Inputs -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=CHECK-MSVC-IR
// RUN: %swift -module-name Swift -target x86_64-unknown-windows-msvc -I %S/Inputs -S %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=CHECK-MSVC-ASM

// REQUIRES: CODEGENERATOR=X86

import AutolinkCoffCPragma

// CHECK-MSVC-IR: !llvm.linker.options = !{!{{[0-9]+}}, !{{[0-9]+}}}
// CHECK-MSVC-IR-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:module.lib"}
// CHECK-MSVC-IR-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:transitive-module.lib"}

// CHECK-MSVC-ASM: .section .drectve
// CHECK-MSVC-ASM-DAG: .ascii " /DEFAULTLIB:module.lib"
// CHECK-MSVC-ASM-DAG: .ascii " /DEFAULTLIB:transitive-module.lib"
