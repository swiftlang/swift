// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
class C {
  func foo() {
    func bar() {
      self.foo()
    }
    // Yes, there are really two arguments called self in this example!
    // CHECK: (name: "self", arg: 1, scope: ![[SCOPE:[0-9]+]], {{.*}}line: 10,
    // CHECK: (name: "self", arg: 2, scope: ![[SCOPE]], {{.*}}line: 3,
    {[weak self] in _ = self!; bar() }()
  }
}
