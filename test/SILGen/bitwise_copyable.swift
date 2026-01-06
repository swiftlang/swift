// RUN: %target-swift-frontend                         \
// RUN:     %s                                         \
// RUN:     -emit-silgen                               \
// RUN:     -target %target-swift-5.1-abi-triple       \
// RUN:     -enable-experimental-feature Sensitive     \
// RUN:     -enable-builtin-module

// REQUIRES: swift_feature_Sensitive

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

import Builtin

func foo() {
  let bricks: Builtin.FixedArray<1, Conditional<Int>>
  let bricks2: Builtin.FixedArray<1, Conditional<String>>
}
