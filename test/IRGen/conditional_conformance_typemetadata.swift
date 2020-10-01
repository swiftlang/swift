// RUN: %target-swift-frontend -O -emit-ir %s
// RUN: %target-swift-frontend -O -primary-file %s -emit-ir

// Make sure that we don't crash in IRGen because the only reference to the type
// E is in the protocol conformance descriptor describing the conditional
// conformace.

private enum E {}

private protocol P { associatedtype AT }

private struct S<A, B> {}

extension S: P where A == E { typealias AT = B }

print(S<E, Bool>.AT.self)
