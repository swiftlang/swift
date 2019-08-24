// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

class Owner {
  var children: [Child] = []

  func addChild(_ c: Child) {
    children.append(c)
  }

  func removeChildren() {
    children.removeAll()
  }

  func test() {
    // Overflow of unowned ref count on 32bit.
    for _ in 0 ..< 500 {
      addChild(Child(self))
    }
    removeChildren()
  }
}

class Child {
  unowned var owner: Owner

  init(_ o: Owner) {
    owner = o
  }
}

let o = Owner()
o.test()
print("success")
// CHECK: success
