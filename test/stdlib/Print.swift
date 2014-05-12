// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: [1, 2, 3]
[1, 2, 3].printSelf()
println()

// CHECK: [foo, bar, bas]
["foo", "bar", "bas"].printSelf()
println()

struct Unprintable {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }
}

// CHECK: [<something>, <something>, <something>]
[Unprintable(1), Unprintable(2), Unprintable(3)].printSelf()
println()

struct CanBePrinted : Printable {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  func printSelf() {
    print("►\(x)◀︎")
  }
}

// CHECK: [►1◀︎, ►2◀︎, ►3◀︎]
[CanBePrinted(1), CanBePrinted(2), CanBePrinted(3)].printSelf()
println()

// CHECK: [(1, two, <something>), (11, twenty-two, <something>), (111, two hundred twenty-two, <something>)]
[(1, "two", Unprintable(3)),
 (11, "twenty-two", Unprintable(33)),
 (111, "two hundred twenty-two", Unprintable(333))].printSelf()
println()
