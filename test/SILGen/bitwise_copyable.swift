// RUN: %target-swift-frontend                                  \
// RUN:     %s                                                  \
// RUN:     -emit-silgen                                        \
// RUN:     -disable-availability-checking                      \
// RUN:     -enable-experimental-feature Sensitive              \
// RUN:     -enable-builtin-module

// REQUIRES: asserts

// Force verification of TypeLowering's isTrivial.

import Builtin

struct S : BitwiseCopyable {
  unowned(unsafe) let c: Builtin.AnyObject
}

struct B<T> {
  var t: T
}

func doit() -> B<Int> {
  return .init(t: 0)
}

struct Conditional<T> {
  var t: T
}
extension Conditional : BitwiseCopyable where T : BitwiseCopyable {}

func doit() -> B<Conditional<Int>> { 
  .init(t: .init(t: 0)) 
}

enum Context<T> {
  struct Here {
    var t: T
  }
}

func doit() -> Context<Int>.Here { .init(t: 0) }

public enum E : BitwiseCopyable {
  case a
}

func take<T : BitwiseCopyable>(_ t: T) {}

func pass(_ e: E) { take(e) }

func opacify() -> some BitwiseCopyable {
    return Int()
}

struct NeverGoingToBeBitwiseCopyable {
  var a: AnyObject
}

@available(*, unavailable)
extension NeverGoingToBeBitwiseCopyable : BitwiseCopyable {
}

struct AlsoNotBitwiseCopyable : ~BitwiseCopyable {}

@sensitive
struct S_Explicit_Sensitive {
}

func takeS_Explicit_Sensitive(_ s: S_Explicit_Sensitive) {
}
