// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: [1, 2, 3]
[1, 2, 3].printSelf()
println()

// CHECK: [foo, bar, bas]
["foo", "bar", "bas"].printSelf()
println()

struct Unprintable {
  val x: Int
}

// CHECK: [<something>, <something>, <something>]
[Unprintable(1), Unprintable(2), Unprintable(3)].printSelf()
println()

struct CanBePrinted : Printable {
  val x: Int
  func printSelf() {
    print("►\(x)◀︎")
  }
}

// CHECK: [►1◀︎, ►2◀︎, ►3◀︎]
[CanBePrinted(1), CanBePrinted(2), CanBePrinted(3)].printSelf()
println()
