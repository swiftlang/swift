// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

protocol MyPrintable {
  func print()
}

struct PrintableValue : MyPrintable {
  init(_ value: String) {
    self.value = value
  }

  func print() {
    Swift.print(value, appendNewline: false)
  }

  var value: String
}

class Foo<T : MyPrintable> {
  init<U : MyPrintable>(_ t: T, _ u: U) {
    print("init ", appendNewline: false)
    t.print()
    print(" ", appendNewline: false)
    u.print()
    print("")
  }

  func bar<U : MyPrintable>(u: U) {
    print("bar ", appendNewline: false)
    u.print()
    print("")
  }
}

// CHECK: init 1 two
var foo = Foo<PrintableValue>(PrintableValue("1"), PrintableValue("two"))
// CHECK: bar 3
var c = PrintableValue("3")
foo.bar(c)
