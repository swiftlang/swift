// RUN: %target-run-simple-swift | FileCheck %s

func test1() {
  print("test1")
  var i : Int
  for i=0;;++i {
    if i > 2 {
      break
    }
    print(i)
    continue
  }
}
func test2() {
  print("test2")
  var i : Int
  for i=0;i<10;++i {
    if i > 2 {
      continue
    }
    print(i)
  }
}
func test3() {
  print("test3")
  var i : Int
  for i=0;i<10;++i {
    if i > 2 {
      break
    }
    print(i)
  }
}
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
      ++i
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
func test8() {
  print("test8")
  var i : Int
  for i=0;;++i {
    for j in 0..<10 {
      if j > 1 {
        break
      }
      print(j)
    }
    if i > 2 {
      break
    }
    print(i)
    continue
  }
}
print("start")
test1()
// CHECK: test1
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
test2()
// CHECK: test2
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
test3()
// CHECK: test3
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
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
test8()
// CHECK: test8
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 0
// CHECK-NEXT: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 1
