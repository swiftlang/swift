// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %S/section.swift -S -parse-as-library | %FileCheck %s --check-prefix=ASM --check-prefix ASM-%target-os
// REQUIRES: CPU=wasm32
// REQUIRES: swift_in_compiler

// Wasm cannot have user defined section name for code sections
//
// ASM-NOT: .section{{.*}}__TEXT,__mysection
// ASM: .section        ".text.$s7section3fooyyF"
// ASM: $s7section3fooyyF:

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

//
// ASM: .section        ".bss.$s7section2g0_Wz"
// ASM: $s7section2g0_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g0Sivp:

// ASM: .section        ".bss.$s7section2g1_Wz"
// ASM: $s7section2g1_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g1Si_Sitvp:

// ASM: .section        ".bss.$s7section2g2_Wz"
// ASM: $s7section2g2_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g2Sbvp:

// ASM: .section        ".bss.$s7section2g3_Wz"
// ASM: $s7section2g3_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g3Sbvp:

// ASM: .section        ".bss.$s7section2g4_Wz"
// ASM: $s7section2g4_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g4SpySiGSgvp:

// ASM: .section        ".bss.$s7section2g5_Wz"
// ASM: $s7section2g5_Wz:
// ASM: .section        "__TEXT,__mysection"
// ASM: $s7section2g5SpySiGSgvp:
