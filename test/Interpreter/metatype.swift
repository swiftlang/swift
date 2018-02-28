// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol Bar : class {
}

public class Foo : Bar {
}

public class Base {
   final fileprivate(set) var a: UInt32 = 0
}

public class Derived<T> : Base {
  final var type : Bar.Type
  final var k = Foo()

  init(_ t: Bar.Type, _ kl: Foo ) {
    type = t
    k = kl
  }
}

public func dontCrash() {
  // CHECK: Derived<Swift.Int>
  print(Derived<Int>(Foo.self, Foo()))
}

dontCrash()
