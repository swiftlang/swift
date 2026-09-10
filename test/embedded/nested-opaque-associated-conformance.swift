// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -Onone -parse-as-library -enable-experimental-feature Embedded -c -o %t/main.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/main.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

// A conformance whose associated type is an opaque result type of a generic type looks
// "dependent" to IRGen, which would need to instantiate the witness table at runtime.
// That's not possible in Embedded Swift - and not needed, because the mandatory pipeline
// specializes all witness tables.

protocol R {
  func r()
}

struct PlainR: R {
  func r() { print("PlainR.r") }
}

struct WrapperR<T>: R {
  var t: T
  func r() { print("WrapperR.r") }
}

protocol Q {
  associatedtype B: R
  func makeB() -> B
}

// The nested opaque type does not depend on `T`.
struct IndependentQ<T>: Q {
  func makeB() -> some R { PlainR() }
}

// The nested opaque type depends on `T`.
struct DependentQ<T>: Q {
  var t: T
  func makeB() -> some R { WrapperR(t: t) }
}

protocol P: AnyObject {
  associatedtype A: Q
  func make() -> A
}

final class C1: P {
  func make() -> some Q { IndependentQ<Int>() }
}

final class C2: P {
  func make() -> some Q { DependentQ(t: 27) }
}

final class C3<T>: P {
  var t: T
  init(t: T) { self.t = t }
  func make() -> some Q { DependentQ(t: t) }
}

@inline(never)
func callThroughExistential(_ p: any P) {
  p.make().makeB().r()
}

@main
struct Main {
  static func main() {
    // CHECK: PlainR.r
    callThroughExistential(C1())
    // CHECK: WrapperR.r
    callThroughExistential(C2())
    // CHECK: WrapperR.r
    callThroughExistential(C3(t: true))
  }
}
