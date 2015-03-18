// RUN: rm -rf %t && mkdir -p %t

// RUN: %target-swiftc_driver -emit-module -module-name test %s -o %t/a.swiftmodule
// RUN: %target-swiftc_driver -emit-sib -module-name test %s -o - | %target-swiftc_driver -emit-module -module-name test -o %t/b.swiftmodule -
// RUN: cmp %t/a.swiftmodule %t/b.swiftmodule
// RUN: cmp %t/a.swiftdoc %t/b.swiftdoc

public struct Pair<A, B> {
  public var first : A
  public var second : B

  public init(a : A, b : B) {
    first = a
    second = b
  }
}

public extension Pair {
  public func swap() -> (B, A) {
    return (second, first)
  }
}

public class MyClass {
  var x : Int

  public init(input : Int) {
    x = 2 * input
  }

  public func do_something(input : Int) -> Int {
    return x * input
  }
}
