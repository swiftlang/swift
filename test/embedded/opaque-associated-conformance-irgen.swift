// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo -module-name test | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

// Check that IRGen looks through opaque result types when it emits the
// associated-conformance entry of a witness table. In Embedded Swift such an entry
// directly points to the witness table of the associated conformance, so the abstract
// conformance of an opaque type has to be resolved to its underlying type.

public protocol R {
  func r()
}

struct TheR: R {
  func r() {}
}

public protocol Q {
  associatedtype B: R
  func q()
  func makeR() -> B
}

struct SomeQ: Q {
  func q() {}
  func makeR() -> some R { TheR() }
}

public protocol P: AnyObject {
  associatedtype A: Q
  func make() -> A
}

public final class C: P {
  public func make() -> some Q { SomeQ() }
}

public func createExistential() -> any P {
  return C()
}

// Both associated-conformance entries are resolved through an opaque type:
// `C.A` -> `SomeQ` and `SomeQ.B` -> `TheR`.

// CHECK-DAG: @"$e4test4TheRVAA1RAAWP" = {{.*}}[2 x ptr] [ptr null, ptr @"$e4test4TheRVAA1RA2aDP1ryyFTW"]
// CHECK-DAG: @"$e4test5SomeQVAA1QAAWP" = {{.*}}[5 x ptr] [ptr null, ptr @"$e4test4TheRVAA1RAAWP",
// CHECK-DAG: @"$e4test1CCAA1PAAWP" = {{.*}}[4 x ptr] [ptr null, ptr @"$e4test5SomeQVAA1QAAWP",
