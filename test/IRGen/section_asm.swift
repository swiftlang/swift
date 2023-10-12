// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %S/section.swift -S -parse-as-library | %FileCheck %s --check-prefix=ASM --check-prefix ASM-%target-os
// REQUIRES: swift_in_compiler
// UNSUPPORTED: CPU=wasm32

// ASM: .section{{.*}}__TEXT,__mysection
// ASM-NOT: .section
// ASM: $s7section3fooyyF:
// ASM-linux-gnu: .section{{.*}}__TEXT,__mysection
// ASM-linux-android: .section{{.*}}__TEXT,__mysection
// ASM-linux-androideabi: .section{{.*}}__TEXT,__mysection
// ASM-NOT: .section
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
