// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -Xllvm -sil-print-transform-blocks=false -parse-as-library -module-name test_only_cast_to_scalar -emit-sil %s | %FileCheck %s

// `x is T` lowers to `checked_cast_addr_br test_only` whenever the types are not
// known to be scalar when SILGen runs, which includes every generic `is`. Once
// specialization or inlining makes `T` a class, that test must become the cheap
// scalar `checked_cast_br` again rather than staying a generic, address-based
// cast that calls into the runtime. Otherwise code like `unsafeDowncast`'s
// precondition, inlined into a generic class's methods, gets slower for no
// reason (this regressed AnyCollection, Array bridging and Dictionary bridging
// benchmarks when `is` was moved to test_only).

public class Base {}
public final class Derived: Base {}

@inline(__always)
func isT<T: AnyObject>(_ x: AnyObject, _: T.Type) -> Bool { x is T }

@inline(__always)
func isSub<T: Base>(_ x: Base, _: T.Type) -> Bool { x is T }

// From AnyObject: the cast cannot forward a borrow, so the test works on a copy.
//
// CHECK-LABEL: sil [noinline] @{{.*}}fromAnyObject{{.*}}
// CHECK-NOT:     test_only
// CHECK:         checked_cast_br AnyObject in %0 : $AnyObject to Derived
// CHECK:       } // end sil function
@inline(never)
public func fromAnyObject(_ x: AnyObject) -> Bool { isT(x, Derived.self) }

// From a class to a subclass: the cast preserves a borrow.
//
// CHECK-LABEL: sil [noinline] @{{.*}}fromBaseClass{{.*}}
// CHECK-NOT:     test_only
// CHECK:         checked_cast_br Base in %0 : $Base to Derived
// CHECK:       } // end sil function
@inline(never)
public func fromBaseClass(_ x: Base) -> Bool { isSub(x, Derived.self) }

public protocol P {}
extension Derived: P {}

// An address-only source stays a generic test: there is no scalar form.
//
// CHECK-LABEL: sil [noinline] @{{.*}}fromExistentialKeep{{.*}}
// CHECK:         checked_cast_addr_br test_only any P in {{%.*}} : $*any P to Derived
// CHECK:       } // end sil function
@inline(never)
public func fromExistentialKeep(_ x: P) -> Bool { x is Derived }
