// RUN: %target-swift-frontend -enable-experimental-feature SymbolLinkageMarkers -primary-file %S/section.swift -S -parse-as-library | %FileCheck %s
// REQUIRES: swift_in_compiler
// UNSUPPORTED: CPU=wasm32

// CHECK: .section{{.*}}__TEXT,__mysection
// CHECK-NOT: .section
// CHECK: $s7section3fooyyF:

// CHECK: .section{{.*}}__TEXT,__mysection
// CHECK-NOT: .section
// CHECK: $s7section8MyStructV3fooyyF:

// CHECK: .section{{.*}}__DATA,__mysection
// CHECK-NOT: .section
// CHECK: $s7section2g0Sivp:
// CHECK-NOT: .section
// CHECK: $s7section2g1Si_Sitvp:
// CHECK-NOT: .section
// CHECK: $s7section2g2Sbvp:
// CHECK-NOT: .section
// CHECK: $s7section2g3Sbvp:
// CHECK-NOT: .section
// CHECK: $s7section2g4SpySiGSgvp:
// CHECK-NOT: .section
// CHECK: $s7section2g5SpySiGSgvp:
// CHECK-NOT: .section
// CHECK: $s7section8MyStructV7static0SivpZ:
