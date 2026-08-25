// RUN: %swift-frontend -S -O %s -target avr-none-none-elf \
// RUN:   -wmo -enable-experimental-feature Embedded | %FileCheck %s
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: CODEGENERATOR=AVR
// REQUIRES: swift_feature_Embedded

// IRGen needs various patches to work with program address space 1 on AVR.
// Even the most basic stub program will fail to lower to IR without them.

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

// CHECK-LABEL: main:
// CHECK: ldi	r16, 0
// CHECK: ldi	r17, 0
// CHECK: ldi	r24, 1
// CHECK: ldi	r25, 0
// CHECK: sts	$e19avrCallbackEmission1iSivp+1, r25
// CHECK: sts	$e19avrCallbackEmission1iSivp, r24
// CHECK: sts	$e19avrCallbackEmission1jSivp+1, r25
// CHECK: sts	$e19avrCallbackEmission1jSivp, r24
// CHECK: sts	$e19avrCallbackEmission05firstB0ySicSgvp+3, r17
// CHECK: sts	$e19avrCallbackEmission05firstB0ySicSgvp+2, r16
// CHECK: ldi	r24, pm_lo8($e19avrCallbackEmissionySicfU_)
// CHECK: ldi	r25, pm_hi8($e19avrCallbackEmissionySicfU_)
// CHECK: sts	$e19avrCallbackEmission05firstB0ySicSgvp+1, r25
// CHECK: sts	$e19avrCallbackEmission05firstB0ySicSgvp, r24
// CHECK: ldi	r24, pm_lo8($e19avrCallbackEmissions5UInt8VACcfU0_To)
// CHECK: ldi	r25, pm_hi8($e19avrCallbackEmissions5UInt8VACcfU0_To)
// CHECK: sts	$e19avrCallbackEmission06secondB0s5UInt8VADXCSgvp+1, r25
// CHECK: sts	$e19avrCallbackEmission06secondB0s5UInt8VADXCSgvp, r24

// CHECK-LABEL: .protected	$e19avrCallbackEmissions5UInt8VACcfU0_To
// CHECK: .globl	$e19avrCallbackEmissions5UInt8VACcfU0_To
// CHECK: .p2align	1
// CHECK: .type	$e19avrCallbackEmissions5UInt8VACcfU0_To,@function
// CHECK-LABEL: $e19avrCallbackEmissions5UInt8VACcfU0_To:
// CHECK: lds	r20, $e19avrCallbackEmission1jSivp
// CHECK: lds	r21, $e19avrCallbackEmission1jSivp+1
// CHECK: ret

