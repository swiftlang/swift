// RUN: %swift -typecheck %s -verify -target avr-none-none -disable-objc-interop -parse-stdlib

// expected-warning@<unknown> * {{no target microcontroller specified, please pass -mmcu=<mcu name>}}

#if arch(avr) && os(none) && _runtime(_Native) && _endian(little) && _pointerBitWidth(_16)
class C {}
var x = C()
#endif
var y = x

