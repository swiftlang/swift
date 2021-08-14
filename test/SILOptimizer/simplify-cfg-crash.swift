// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// Check that the optimizer does not crash.


public class Base {
  @inline(never)
  final func next() -> Base? {
    return self
  }
}

public class Derived : Base {}

// CHECK: sil {{.*}}testit
public func testit(_ x: Base?) -> Derived? {
    var i: Base? = x
    while (i is Derived) == false && i != nil {
        i = i?.next()
    }
    return i as? Derived
}

