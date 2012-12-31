// RUN: %swift %s -i | FileCheck %s

func test() {
  var d1 = new Dictionary
  var d2 = new Dictionary
  var d1_alias = d1
  if d1 != d2 {
    println("good")
  }
  if d1 == d1_alias {
    println("also good")
  }
}

test()

// CHECK: good
// CHECK: also good
