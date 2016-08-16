// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func test4() {
  print("test4")
  for i in 0..<10 {
    if i > 2 {
      break
    }
    print(i)
  }
}
func test5() {
  print("test5")
  for i in 0..<10 {
    if i < 2 {
      print(i)
      continue
    }
    return
  }
}
func test6() {
  print("test6")
  var i = 0
  while (i < 10) {
    if i < 2 {
      print(i)
      i += 1
      continue
    }
    return
  }
}
func test7() {
  print("test7")
  var i = 0
  while (i < 10) {
    if i < 2 {
      print(i)
      break
    }
    return
  }
  print("foo")
}
print("start")

test4()
// CHECK: test4
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
test5()
// CHECK: test5
// CHECK-NEXT: 0
// CHECK-NEXT: 1
test6()
// CHECK: test6
// CHECK-NEXT: 0
// CHECK-NEXT: 1
test7()
// CHECK: test7
// CHECK-NEXT: 0
// CHECK-NEXT: foo
