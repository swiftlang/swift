// RUN: %empty-directory(%t)

// RUN: %target-swiftc_driver -emit-module -module-name test %s -o %t/a.swiftmodule
// RUN: %target-swiftc_driver -emit-sib -module-name test %s -o - | %target-swiftc_driver -emit-module -module-name test -o %t/b.swiftmodule -

// RUN: mkdir -p %t/a/
// RUN: cp %t/a.swiftmodule %t/a/test.swiftmodule
// RUN: mkdir -p %t/b/
// RUN: cp %t/b.swiftmodule %t/b/test.swiftmodule

// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=test -I %t/a -source-filename=%s > %t.a.swift.txt
// RUN: %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=test -I %t/b -source-filename=%s > %t.b.swift.txt
// RUN: diff -u %t.a.swift.txt %t.b.swift.txt

// Diff the SIL
// RUN: %target-swift-frontend -emit-sil %t/a/test.swiftmodule > %t.a.sil.txt
// RUN: %target-swift-frontend -emit-sil %t/b/test.swiftmodule > %t.b.sil.txt
// RUN: diff -u %t.a.sil.txt %t.b.sil.txt


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
