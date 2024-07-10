// RUN: %swift-frontend -emit-ir %s -target avr-none-none-elf \
//   -wmo -enable-experimental-feature Embedded
// REQUIRES: embedded_stdlib_cross_compiling

import Swift

#if arch(avr) && os(none) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_16)
let i: Int = 1
#endif

var j = i
