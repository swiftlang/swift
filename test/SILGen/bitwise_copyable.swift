// RUN: %target-swift-frontend                                  \
// RUN:     %s                                                  \
// RUN:     -emit-silgen                                        \
// RUN:     -disable-availability-checking                      \
// RUN:     -enable-experimental-feature ConformanceSuppression \
// RUN:     -enable-experimental-feature BitwiseCopyable        \
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

struct Conditional<T> {
  var t: T
}
extension Conditional : _BitwiseCopyable where T : _BitwiseCopyable {}

func doit() -> B<Conditional<Int>> { 
  .init(t: .init(t: 0)) 
}

enum Context<T> {
  struct Here {
    var t: T
  }
}

func doit() -> Context<Int>.Here { .init(t: 0) }

public enum E : _BitwiseCopyable {
  case a
}

func take<T : _BitwiseCopyable>(_ t: T) {}

func pass(_ e: E) { take(e) }

func opacify() -> some _BitwiseCopyable {
    return Int()
}

struct NeverGoingToBeBitwiseCopyable {
  var a: AnyObject
}

@available(*, unavailable)
extension NeverGoingToBeBitwiseCopyable : _BitwiseCopyable {
}

struct AlsoNotBitwiseCopyable : ~_BitwiseCopyable {}
