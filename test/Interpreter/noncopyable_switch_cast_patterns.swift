// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// `switch consume x { case let v as T: ... }` over a non-`Copyable` subject.
// The cases are tested under a borrow and the matching case alone consumes the
// subject, so a case that does not match must leave it intact for later cases.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Small: P, ~Copyable { var tag: Int }
struct Big: P, ~Copyable { var a: Int; var b: Int; var c: Int }
struct Unrelated: Q, ~Copyable { var tag: Int }

final class Canary {
  static var deinitCount = 0
  deinit { Canary.deinitCount += 1 }
}

struct WithCanary: P, ~Copyable {
  let canary: Canary
  var tag: Int
}

// Existential subject: only the matching case runs, and it owns the payload.
func viaExistential(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case let u as Unrelated: return u.tag
  case let b as Big: return b.a + b.b + b.c
  case let s as Small: return s.tag
  default: return -1
  }
}

// A concrete subject. The statically-satisfied cast matches unconditionally.
func viaConcrete(_ box: consuming Small) -> Int {
  switch consume box {
  case let u as Unrelated: return -u.tag
  case let s as Small: return s.tag
  default: return -1
  }
}

func viaErasure(_ box: consuming Small) -> Int {
  switch consume box {
  case let u as Unrelated: return -u.tag
  case let p as (P & ~Copyable): _ = p; return 7
  default: return -1
  }
}

func viaGeneric<T: ~Copyable>(_ box: consuming T) -> Int {
  switch consume box {
  case let s as Small: return s.tag
  case let b as Big: return b.a
  default: return -1
  }
}

// No case matches: the subject is dropped exactly once.
func viaNoMatch(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case let u as Unrelated: return u.tag
  default: return -1
  }
}

// CHECK: existential: 3 6 5 -1
print("existential:", viaExistential(Small(tag: 3)),
      viaExistential(Big(a: 1, b: 2, c: 3)),
      viaExistential(Small(tag: 5)),
      viaExistential(WithCanary(canary: Canary(), tag: 9)))
// CHECK: concrete: 4
print("concrete:", viaConcrete(Small(tag: 4)))
// CHECK: erasure: 7
print("erasure:", viaErasure(Small(tag: 4)))
// CHECK: generic: 8 2 -1
print("generic:", viaGeneric(Small(tag: 8)), viaGeneric(Big(a: 2, b: 0, c: 0)),
      viaGeneric(Unrelated(tag: 1)))

// Each value is destroyed exactly once, whether it was bound or dropped.
Canary.deinitCount = 0
_ = viaNoMatch(WithCanary(canary: Canary(), tag: 1))
// CHECK: no match deinits: 1
print("no match deinits:", Canary.deinitCount)

func bindsCanary(_ box: consuming any P & ~Copyable) -> Int {
  switch consume box {
  case let w as WithCanary: return w.tag
  default: return -1
  }
}
Canary.deinitCount = 0
// CHECK: bound: 9 deinits: 1
print("bound:", bindsCanary(WithCanary(canary: Canary(), tag: 9)),
      "deinits:", Canary.deinitCount)
