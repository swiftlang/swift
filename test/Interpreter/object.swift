// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

func test() {
  var d1 = Dictionary<String, Int>()
  var d2 = Dictionary<String, Int>()
  var d1_alias = d1
//  Dictionary is no longer a class type
//  if d1 !== d2 {
    print("good")
//  }
//  if d1 === d1_alias {
    print("also good")
//  }
}

test()

// CHECK: good
// CHECK: also good
