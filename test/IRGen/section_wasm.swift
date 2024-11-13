// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %S/section.swift -S -parse-as-library | %FileCheck %s --check-prefix=ASM --check-prefix ASM-%target-os
// REQUIRES: CPU=wasm32
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_SymbolLinkageMarkers

// Wasm cannot have user defined section name for code sections
//
// ASM-NOT: .section{{.*}}__TEXT,__mysection
// ASM: .section        ".text.$s7section3fooyyF"
// ASM: $s7section3fooyyF:
// ASM: .section        ".text.$s7section8MyStructV3fooyyF","R",@
// ASM: $s7section8MyStructV3fooyyF:

// Wasm places one-time init token on .bss section before each "__TEXT,__mysection"
// so need to put .section directives for each data to switch sections.
// Here is an example pattern of a global variable with @section
// ```
//         .type   $s7section2g0_Wz,@object
//         .section        ".bss.$s7section2g0_Wz","",@
//         .p2align        2, 0x0
// $s7section2g0_Wz:
//         .int32  0
//         .size   $s7section2g0_Wz, 4
//
//         .hidden $s7section2g0Sivp
//         .type   $s7section2g0Sivp,@object
//         .section        "__TEXT,__mysection","",@
//         .globl  $s7section2g0Sivp
//         .p2align        2, 0x0
// $s7section2g0Sivp:
//         .int32  1
//         .size   $s7section2g0Sivp, 4
// ```

// ASM: .section        "__DATA,__mysection"
// ASM: $s7section2g0Sivp:
// ASM-NOT: .section
// ASM: $s7section2g1Si_Sitvp:
// ASM-NOT: .section
// ASM: $s7section2g2Sbvp:
// ASM-NOT: .section
// ASM: $s7section2g3Sbvp:
// ASM-NOT: .section
// ASM: $s7section2g4SpySiGSgvp:
// ASM-NOT: .section
// ASM: $s7section2g5SpySiGSgvp:
