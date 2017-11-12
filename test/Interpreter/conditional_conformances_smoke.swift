// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

// Smoke test to see that various conditional conformances correctly execute

protocol P1 {
  func method1()
}
var isp1_count = 0
struct IsP1: P1 {
  var id: Int
  init() {
    id = isp1_count
    isp1_count += 1
  }
  func method1() {
    print("IsP1", id)
  }
}
protocol P2 {
  func method2()
}
protocol P3 {
  func method3()
}
protocol P4: P1 {
  func method4()
}

protocol Assoc { associatedtype AT }

func takes_P2<X: P2>(_ x: X) {
  x.method2()
}

struct NoRecur<T> {
  var x: T
}
extension NoRecur: P2 where T: P1 {
  func method2() {
    print("NoRecur")
    x.method1()
  }
}

var noRecur = NoRecur(x: IsP1())
print("noRecur.method2()")
noRecur.method2()
// CHECK-LABEL: noRecur.method2()
// CHECK-NEXT: NoRecur
// CHECK-NEXT: IsP1 0

print("takes_P2(NoRecur(...))")
takes_P2(NoRecur(x: IsP1()))
// CHECK-LABEL: takes_P2(NoRecur(...))
// CHECK-NEXT: NoRecur
// CHECK-NEXT: IsP1 1

struct Recur<T> {
  var id: Int
  var x: T
}
extension Recur: P2 where T: P2 {
  func method2() {
    print("Recur", id)
    x.method2()
  }
}

var recur = Recur(id: 0, x: Recur(id: 1, x: NoRecur(x: IsP1())))
print("recur.method2()")
recur.method2()
// CHECK-LABEL: recur.method2()
// CHECK-NEXT: Recur 0
// CHECK-NEXT: Recur 1
// CHECK-NEXT: NoRecur
// CHECK-NEXT: IsP1 2

print("takes_P2(Recur(...))")
takes_P2(Recur(id: 2, x: Recur(id: 3, x: NoRecur(x: IsP1()))))
// CHECK-LABEL: takes_P2(Recur(...))
// CHECK-NEXT: Recur 2
// CHECK-NEXT: Recur 3
// CHECK-NEXT: NoRecur
// CHECK-NEXT: IsP1 3
