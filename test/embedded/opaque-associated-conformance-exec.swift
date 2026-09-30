// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -Onone -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// If an associated type is an opaque result type, the associated conformance is abstract.
// In Embedded Swift the underlying type is always known, so both the witness table
// specialization and IRGen have to look through the opaque type.

protocol Q {
  func q()
}

struct PlainQ: Q {
  func q() { print("PlainQ.q") }
}

struct GenericQ<T>: Q {
  var t: T
  func q() { print("GenericQ.q") }
}

protocol P: AnyObject {
  associatedtype A: Q
  func make() -> A
}

// The underlying type of the opaque type is not generic.
final class OpaquePlain: P {
  func make() -> some Q { PlainQ() }
}

// The underlying type of the opaque type is generic, so it needs a specialized witness
// table which nothing else creates.
final class OpaqueGeneric: P {
  func make() -> some Q { GenericQ(t: 27) }
}

// The same, from a generic class.
final class OpaqueFromGenericClass<T>: P {
  var t: T
  init(t: T) { self.t = t }
  func make() -> some Q { GenericQ(t: t) }
}

@inline(never)
func callThroughExistential(_ p: any P) {
  p.make().q()
}

@main
struct Main {
  static func main() {
    // CHECK: PlainQ.q
    callThroughExistential(OpaquePlain())
    // CHECK: GenericQ.q
    callThroughExistential(OpaqueGeneric())
    // CHECK: GenericQ.q
    callThroughExistential(OpaqueFromGenericClass(t: true))
  }
}
