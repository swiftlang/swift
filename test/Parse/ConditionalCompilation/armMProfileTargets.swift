// RUN: %swift -typecheck %s -verify -target armv6m-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv7m-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv7em-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv8m.base-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv8m.main-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv8.1m.main-none-none-eabi -parse-stdlib
// RUN: %swift -typecheck %s -verify -target armv7em-apple-none-macho -parse-stdlib

#if _hasAtomicBitWidth(_64) || _hasAtomicBitWidth(_128)
// This block should not be type checked.
// M-profile cores only have word-sized atomics.
let i: Int = "Hello"
#endif

#if arch(arm) && os(none) && _pointerBitWidth(_32)
#if _hasAtomicBitWidth(_8) && _hasAtomicBitWidth(_16) && _hasAtomicBitWidth(_32)
#warning("word-sized atomics are available") // expected-warning {{word-sized atomics are available}}
#endif
#endif
