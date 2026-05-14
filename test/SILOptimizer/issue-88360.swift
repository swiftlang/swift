// RUN: %target-swift-emit-sil -primary-file %s -O -module-name=test | %FileCheck %s

// Regression test for https://github.com/swiftlang/swift/issues/88360

open class Base {
  public init() {}
}

open class Intermediate: Base {
  open func foo() { fatalError("abstract") }
}

struct PImpl {
  var base: Base

  // CHECK-LABEL: sil hidden @$s4test5PImplV3fooyyF : $@convention(method) (@inout PImpl) -> ()
  // CHECK-NOT: strong_retain
  // CHECK-NOT: strong_release
  // CHECK: } // end sil function '$s4test5PImplV3fooyyF'
  mutating func foo() {
    guard isKnownUniquelyReferenced(&base) else {
      fatalError("some fallback logic")
    }
    unsafeDowncast(base, to: Intermediate.self).foo()
  }
}
