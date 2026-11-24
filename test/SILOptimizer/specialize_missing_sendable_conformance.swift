// RUN: %target-swift-frontend -O -swift-version 5 -emit-sil %s | %FileCheck %s

// A generic caller that passes a non-Sendable T where Sendable is required has
// a "missing" T: Sendable conformance. Specializing the caller with T := Int
// must produce the real Int: Sendable conformance, so that the specialization
// of the callee matches the one created from a direct Int caller.

public struct Group<C: Sendable> {
  var items: [C] = []
}

@inline(never)
func withGroup<C: Sendable, R>(
  of: C.Type, body: (inout Group<C>) throws -> R
) rethrows -> R {
  var g = Group<C>()
  return try body(&g)
}

@inline(never)
func useInt(_ x: [Int]) -> [Int] {
  withGroup(of: Int.self) { g in g.items = x; return g.items + x }
}

@inline(never)
func useGeneric<T>(_ x: [T]) -> [T] {
  withGroup(of: T.self) { g in x + x }
}

// CHECK-LABEL: sil @$s{{.*}}6caller
public func caller() -> [Int] { useInt([3]) + useGeneric([1, 2]) }
