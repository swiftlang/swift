// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -Onone -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// A witness table which is not specialized itself can still have a specialized associated
// conformance. In Embedded Swift the associated-conformance entry directly points to the
// witness table of that conformance, so it has to be specialized here - nothing else does
// it, because the outer conformance is not specialized.

protocol Q {
  func q()
}

struct GenericQ<T>: Q {
  var t: T
  func q() { print("GenericQ.q") }
}

protocol P: AnyObject {
  associatedtype A: Q
  func make() -> A
}

final class NonGenericClass: P {
  func make() -> GenericQ<Int> { GenericQ(t: 27) }
}

// The same via a base protocol, so that the base-protocol entry is covered, too.
protocol P2: P {}

final class ViaBaseProtocol: P2 {
  func make() -> GenericQ<Bool> { GenericQ(t: true) }
}

@inline(never)
func callThroughExistential(_ p: any P) {
  p.make().q()
}

@main
struct Main {
  static func main() {
    // CHECK: GenericQ.q
    callThroughExistential(NonGenericClass())
    // CHECK: GenericQ.q
    callThroughExistential(ViaBaseProtocol())
  }
}
