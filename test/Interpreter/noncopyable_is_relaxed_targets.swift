// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// The cast classes that Sema used to reject for `is`, answered at runtime.
//
// `as?`/`as!` have to produce the payload, which a non-`Copyable` type cannot
// survive; `is` produces a `Bool` and SILGen answers it with a non-consuming
// test. These are the three classes the old guard turned away:
//
//   A. non-`Copyable` existential source -> existential target
//   B. concrete non-`Copyable` source -> anything
//   C. any source -> non-`Copyable` target
//
// The point of running them is that several were also mis-diagnosed as
// "always fails" while the runtime answers `true`, so a compile-only test
// would not have caught the error.
//
// The `AnyObject` cases rely on `__SwiftValue` boxing and need Objective-C
// interop; they are in noncopyable_is_relaxed_targets_objc.swift.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct OnlyP: P, ~Copyable { var t = 1 }
struct Both: P, Q, ~Copyable { var t = 2 }
struct Copy: P { var t = 3 }

// MARK: - A. existential source, existential target

func isQ(_ x: consuming any P & ~Copyable) -> Bool { x is any Q & ~Copyable }
func isCopyableP(_ x: consuming any P & ~Copyable) -> Bool { x is any P }
func isAny(_ x: consuming any P & ~Copyable) -> Bool { x is Any }

// A type conforming to both protocols makes this a real question rather than
// the statically-failing one the old diagnostic claimed.
// CHECK: A OnlyP is any Q: false
print("A OnlyP is any Q:", isQ(OnlyP() as any P & ~Copyable))
// CHECK: A Both  is any Q: true
print("A Both  is any Q:", isQ(Both() as any P & ~Copyable))

// `any P` requires a Copyable payload, so only the copyable ones qualify.
// CHECK: A OnlyP is any P: false
print("A OnlyP is any P:", isCopyableP(OnlyP() as any P & ~Copyable))
// CHECK: A Copy  is any P: true
print("A Copy  is any P:", isCopyableP(Copy() as any P & ~Copyable))

// `Any` likewise.
// CHECK: A OnlyP is Any: false
print("A OnlyP is Any:", isAny(OnlyP() as any P & ~Copyable))
// CHECK: A Copy  is Any: true
print("A Copy  is Any:", isAny(Copy() as any P & ~Copyable))

// MARK: - B. concrete noncopyable source

func concreteIsErased(_ x: consuming OnlyP) -> Bool { x is any P & ~Copyable }
func concreteIsOther(_ x: consuming OnlyP) -> Bool { x is Both }

// CHECK: B OnlyP is any P & ~Copyable: true
print("B OnlyP is any P & ~Copyable:", concreteIsErased(OnlyP()))
// CHECK: B OnlyP is Both: false
print("B OnlyP is Both:", concreteIsOther(OnlyP()))

// MARK: - C. noncopyable target from a copyable source

func copyableIsNoncopyable(_ x: consuming any P) -> Bool { x is OnlyP }
func copyableIsNoncopyableExistential(_ x: consuming any P) -> Bool {
  x is any P & ~Copyable
}

// A `Copyable` existential cannot hold a non-`Copyable` payload, so a
// concrete non-`Copyable` target can never match...
// CHECK: C any P is OnlyP: false
print("C any P is OnlyP:", copyableIsNoncopyable(Copy() as any P))
// ... but the *erasure* to a non-`Copyable` existential does, because a
// Copyable payload inhabits one perfectly well.
// CHECK: C any P is any P & ~Copyable: true
print("C any P is any P & ~Copyable:",
      copyableIsNoncopyableExistential(Copy() as any P))

// MARK: - generic parameters, which used to be rejected outright

func isT<T: ~Copyable>(_ x: consuming any P & ~Copyable, _: T.Type) -> Bool {
  x is T
}
func isConcreteFromS<S: ~Copyable>(_ x: consuming S) -> Bool { x is OnlyP }

// CHECK: G OnlyP is T=OnlyP: true
print("G OnlyP is T=OnlyP:", isT(OnlyP() as any P & ~Copyable, OnlyP.self))
// CHECK: G OnlyP is T=Both: false
print("G OnlyP is T=Both:", isT(OnlyP() as any P & ~Copyable, Both.self))
// CHECK: G S=OnlyP is OnlyP: true
print("G S=OnlyP is OnlyP:", isConcreteFromS(OnlyP()))
// CHECK: G S=Both is OnlyP: false
print("G S=Both is OnlyP:", isConcreteFromS(Both()))

// MARK: - the three spellings must agree

func exprForm(_ x: consuming any P & ~Copyable) -> Bool { x is any Q & ~Copyable }
func switchForm(_ x: consuming any P & ~Copyable) -> Bool {
  switch x { case is any Q & ~Copyable: return true; default: return false }
}
func ifCaseForm(_ x: consuming any P & ~Copyable) -> Bool {
  if case is any Q & ~Copyable = x { return true }
  return false
}

// CHECK: agree true true true
print("agree", exprForm(Both() as any P & ~Copyable),
      switchForm(Both() as any P & ~Copyable),
      ifCaseForm(Both() as any P & ~Copyable))
// CHECK: agree false false false
print("agree", exprForm(OnlyP() as any P & ~Copyable),
      switchForm(OnlyP() as any P & ~Copyable),
      ifCaseForm(OnlyP() as any P & ~Copyable))
