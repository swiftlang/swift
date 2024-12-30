// RUN: %swift-frontend -emit-ir %s -target avr-none-none-elf \
// RUN:   -wmo -enable-experimental-feature Embedded | %FileCheck %s
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: CODEGENERATOR=AVR
// REQUIRES: swift_feature_Embedded

// IRGen needs various patches to work with program address space 1 on AVR.
// Even the most basic stub program will fail to lower to IR without them.
// We test IR rather than object code for now, to avoid any issues with an
// out of date LLVM AVR back end throwing off the tests.
// We are focusing on getting the front end functioning.

import Swift

#if arch(avr) && os(none) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_16)
let i: Int = 1
#endif

var j = i

// CHECK: define protected i32 @main(i32 %0, ptr %1) addrspace(1)
