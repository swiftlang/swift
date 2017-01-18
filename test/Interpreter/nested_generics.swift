// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

protocol MyPrintable {
  func print()
}

extension Int : MyPrintable {
  func print() {
    Swift.print(self, terminator: "")
  }
}

extension String : MyPrintable {
  func print() {
    Swift.print(self, terminator: "")
  }
}

extension Array : MyPrintable {
  func print() {
    Swift.print(self, terminator: "")
  }
}

class Foo<T : MyPrintable> {
  init<U : MyPrintable>(_ t: T, _ u: U) {
    print("init ", terminator: "")
    t.print()
    print(" ", terminator: "")
    u.print()
    print("")
  }

  func bar<U : MyPrintable>(_ u: U) {
    print("bar ", terminator: "")
    u.print()
    print("")
  }
}

// CHECK: init 1 two
// CHECK: bar [1]
var foo = Foo<Int>(1, "two")
foo.bar([1])

struct OuterStruct<T : MyPrintable> {
  let t: T

  struct InnerStruct<U : MyPrintable> {
    let u: U

    func printBoth(t: T) {
      t.print()
      print(" ", terminator: "")
      u.print()
    }

    static func printBoth(t: T, u: U) {
      t.print()
      print(" ", terminator: "")
      u.print()
    }

    func printAllThree<V : MyPrintable>(t: T, v: V) {
      printBoth(t: t)
      print(" ", terminator: "")
      v.print()
    }
  }

  class InnerClass<U : MyPrintable> {
    let u: U

    init(u: U) {
      self.u = u
    }

    func printBoth(t: T) {
      t.print()
      print(" ", terminator: "")
      u.print()
    }

    static func printBoth(t: T, u: U) {
      t.print()
      print(" ", terminator: "")
      u.print()
    }

    func printAllThree<V : MyPrintable>(t: T, v: V) {
      printBoth(t: t)
      print(" ", terminator: "")
      v.print()
    }
  }
}

class SubClass<X : MyPrintable, Y : MyPrintable> : OuterStruct<Y>.InnerClass<X> {
  override func printBoth(t: Y) {
    print("override ", terminator: "")
    super.printBoth(t: t)
  }

  // FIXME: Does not work!
  /* override func printAllThree<Z : MyPrintable>(t: Y, v: Z) {
    print("super ", terminator: "")
    super.printAllThree(t: t, v: v)
  } */
}

// CHECK: 1 two
// CHECK: 1 two
// CHECK: 1 two [3]
OuterStruct<Int>.InnerStruct<String>(u: "two").printBoth(t: 1)
OuterStruct<Int>.InnerStruct<String>.printBoth(t: 1, u: "two")
OuterStruct<Int>.InnerStruct<String>(u: "two").printAllThree(t: 1, v: [3])

// CHECK: override 1 two [3]
SubClass<String, Int>(u: "two").printAllThree(t: 1, v: [3])
