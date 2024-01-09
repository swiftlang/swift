// RUN: %target-swift-frontend                           \
// RUN:     %s                                           \
// RUN:     -emit-silgen                                 \
// RUN:     -disable-availability-checking               \
// RUN:     -enable-experimental-feature BitwiseCopyable \
// RUN:     -enable-builtin-module

// REQUIRES: asserts

// Force verification of TypeLowering's isTrivial.

import Builtin

struct S : _BitwiseCopyable {
  unowned(unsafe) let c: Builtin.AnyObject
}

struct B<T> {
  var t: T
}

func doit() -> B<Int> {
  return .init(t: 0)
}
