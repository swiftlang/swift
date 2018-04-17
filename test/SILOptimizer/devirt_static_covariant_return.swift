// RUN: %target-swift-frontend -O -whole-module-optimization %s -emit-sil -sil-verify-all | %FileCheck %s

// CHECK-NOT: class_method

// Static method returning Self in generic class
public class Factory<T> {
  public required init() {}

  @inline(never) class func bar() -> Self {
    return self.init()
  }

  @inline(never) class func foo() -> Self {
    return bar()
  }
}

public func foo(m: Factory<Int>.Type) {
  m.foo()
}
