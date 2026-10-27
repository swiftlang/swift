// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A cast pattern that the subject's static type already satisfies (an identity
// cast, or erasure to a protocol it conforms to) is a Coercion: there is
// nothing to test. In a `switch` over a non-`Copyable` subject these used to
// crash SILGen:
//
// * `switch consume x { case is <statically true>: }` reached
//   unbindAndEndBorrows with an `is` pattern ("bad pattern kind");
// * a binding identity cast whose binding is needed before the consuming phase
//   (a borrowing `switch`, or a `where` guard) asserted in bindBorrow.
//
// Several of these shapes worked in Swift 6.4. An identity cast now binds the
// subject itself.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

final class Canary {
  static var live = 0
  init() { Canary.live += 1 }
  deinit { Canary.live -= 1 }
}

struct Pay: P, ~Copyable { let canary = Canary(); var tag: Int }
struct Other: Q, ~Copyable { var tag: Int }

func make(_ tag: Int) -> any P & ~Copyable { Pay(tag: tag) }

// MARK: - consuming switch, statically true `is`

func consumeIsExistential(_ x: consuming any P & ~Copyable) -> Int {
  switch consume x {
  case is Other: return -1
  case is any P & ~Copyable: return 1
  default: return 0
  }
}

func consumeIsConcrete(_ x: consuming Pay) -> Int {
  switch consume x {
  case is Pay: return 2
  default: return 0
  }
}

// CHECK: consuming is: 1 2
print("consuming is:", consumeIsExistential(make(1)),
      consumeIsConcrete(Pay(tag: 1)))

// MARK: - identity binding with a `where` guard

func guardedExistential(_ x: consuming any P & ~Copyable) -> Int {
  switch consume x {
  case let b as any P & ~Copyable where true: _ = b; return 1
  default: return 0
  }
}

func guardedConcrete(_ x: consuming Pay) -> Int {
  switch consume x {
  case let b as Pay where b.tag == 2: return 20
  case let b as Pay: return b.tag
  }
}

// CHECK: guarded: 1 20 3
print("guarded:", guardedExistential(make(1)), guardedConcrete(Pay(tag: 2)),
      guardedConcrete(Pay(tag: 3)))

// MARK: - identity binding in a borrowing switch

func borrowedExistential(_ x: borrowing any P & ~Copyable) -> Int {
  switch x {
  case let b as any P & ~Copyable: _ = b; return 1
  }
}

// The subject is still usable after a borrowing switch.
func borrowTwice(_ x: borrowing any P & ~Copyable) -> Int {
  borrowedExistential(x) + borrowedExistential(x)
}

// CHECK: borrowed: 1 2
print("borrowed:", borrowedExistential(make(1)), borrowTwice(make(1)))

// Every payload was destroyed exactly once.
// CHECK: live: 0
print("live:", Canary.live)
