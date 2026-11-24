// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A copyable type may conform to a ~Copyable protocol, so a noncopyable
// existential can hold a payload that reaches conversions no metadata check can
// decide -- ObjC bridging most notably. `"hi" is NSString` is true by bridging
// even though String.self is not a subtype of NSString.self.
//
// swift_dynamicCastTest handles that by delegating any copyable payload to the
// ordinary swift_dynamicCast (into a scratch buffer it then destroys), and only
// answering from metadata when the payload is noncopyable and therefore cannot
// reach those conversions at all. This file pins down the delegating half: if it
// regressed to a metadata-only check, every bridging answer below would flip to
// false.

import Foundation

protocol P: ~Copyable {}

extension String: P {}
extension Int: P {}

struct NC: ~Copyable, P {
  var tag: Int
}

func isNSString(_ box: borrowing any P & ~Copyable) -> Bool { box is NSString }
func isNSNumber(_ box: borrowing any P & ~Copyable) -> Bool { box is NSNumber }

// MARK: - Bridging

// The divergence that rules out a metadata-only implementation.
// CHECK: String payload is NSString: true
print("String payload is NSString:", isNSString("hi" as any P & ~Copyable))

// CHECK: Int payload is NSString: false
print("Int payload is NSString:", isNSString(42 as any P & ~Copyable))

// CHECK: Int payload is NSNumber: true
print("Int payload is NSNumber:", isNSNumber(42 as any P & ~Copyable))

// A noncopyable payload cannot bridge, so this is false -- and, importantly,
// reaching that answer must not have tried to copy it.
// CHECK: NC payload is NSString: false
print("NC payload is NSString:", isNSString(NC(tag: 1) as any P & ~Copyable))

// MARK: - Other value-producing conversions on the copyable path

func isAnyHashable(_ box: borrowing any P & ~Copyable) -> Bool {
  box is AnyHashable
}

// CHECK: String payload is AnyHashable: true
print("String payload is AnyHashable:",
      isAnyHashable("hi" as any P & ~Copyable))

// A noncopyable payload cannot be boxed into AnyHashable, whose init requires a
// Copyable H -- which is what lets the metadata-only path skip that conversion.
// CHECK: NC payload is AnyHashable: false
print("NC payload is AnyHashable:",
      isAnyHashable(NC(tag: 2) as any P & ~Copyable))

// MARK: - The delegating path must still not consume the subject

enum Counter {
  static var deinits = 0
}

final class Canary {
  deinit { Counter.deinits += 1 }
}

// Copyable, carries a class reference, and conforms to the ~Copyable protocol.
// The delegating path copies this into a scratch buffer and destroys it, which
// must leave the original's refcount untouched.
struct CopyableWithCanary: P {
  var canary: Canary
}

func delegatingPathDoesNotConsume() -> Int {
  let before = Counter.deinits
  do {
    let box: any P & ~Copyable = CopyableWithCanary(canary: Canary())
    for _ in 0 ..< 10 {
      if !(box is CopyableWithCanary) { return -1 }
      if box is NSString { return -2 }
    }
    // Nothing has been released yet: the scratch copies were balanced.
    if Counter.deinits != before { return -3 }
  }
  return Counter.deinits - before
}
// CHECK: delegating path, canary destroyed exactly once: 1
print("delegating path, canary destroyed exactly once:",
      delegatingPathDoesNotConsume())

// CHECK: done
print("done")
