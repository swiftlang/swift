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

var firstCallback: ((Int) -> Void)?

var secondCallback: (@convention(c) (UInt8) -> UInt8)?

#endif

var j = i

firstCallback = {
	j += $0
}

secondCallback = {
	return $0 * 2 - UInt8(j)
}


// CHECK: target triple = "avr-none-none-elf"
// CHECK-DAG: 20testIRGenFunctioning1iSivp
// CHECK-DAG: @"$e20testIRGenFunctioning13firstCallbackySicSgvp"
// CHECK-DAG: @"$e20testIRGenFunctioning14secondCallbacks5UInt8VADXCSgvp"
// CHECK-DAG: 20testIRGenFunctioning1jSivp
// CHECK: define protected i32 @main(i32 %0, ptr %1) addrspace(1)
// CHECK: store i16 ptrtoint (ptr addrspace(1) @"$e20testIRGenFunctioningySicfU_" to i16), ptr @"$e20testIRGenFunctioning13firstCallbackySicSgvp"
// CHECK: define protected void @"$e20testIRGenFunctioningySicfU_"(i16 %0) addrspace(1)
