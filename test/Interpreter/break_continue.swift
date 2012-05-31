// RUN: %swift -i %s | FileCheck %s

func test1() {
  println("test1")
  var i : Int
  for i=0;;++i {
    if i > 2 {
      break
    }
    println(i)
    continue
  }
}
func test2() {
  println("test2")
  var i : Int
  for i=0;i<10;++i {
    if i > 2 {
      continue
    }
    println(i)
  }
}
func test3() {
  println("test3")
  var i : Int
  for i=0;i<10;++i {
    if i > 2 {
      break
    }
    println(i)
  }
}
func test4() {
  println("test4")
  for i in 0..10 {
    if i > 2 {
      break
    }
    println(i)
  }
}
func test5() {
  println("test5")
  for i in 0..10 {
    if i < 2 {
      println(i)
      continue
    }
    return
  }
}
func test6() {
  println("test6")
  var i = 0
  while (i < 10) {
    if i < 2 {
      println(i)
      ++i
      continue
    }
    return
  }
}
func test7() {
  println("test7")
  var i = 0
  while (i < 10) {
    if i < 2 {
      println(i)
      break
    }
    return
  }
  println("foo")
}
func test8() {
  println("test8")
  var i : Int
  for i=0;;++i {
    for j in 0..10 {
      if j > 1 {
        break
      }
      println(j)
    }
    if i > 2 {
      break
    }
    println(i)
    continue
  }
}
println("start")
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
