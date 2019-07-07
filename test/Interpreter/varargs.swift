// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

func vf(_ params: CVarArg...) {
	print("OK")
}

var a: [AnyObject]! = ["a" as NSString]
var s: String! = "s"

vf(a as NSArray)
// CHECK: OK
vf(s as NSString)
// CHECK: OK

func f(_ x: Int...) {
  print(#variadic(x), separator: ",")
}

func f(_ x: [Int]...) {
  print(#variadic(x), separator: ",")
}

let closure: ((Int...) -> Void) = {
  (args: Int...) in
  print(#variadic(args), separator: ",")
}

f(1,2,3) // CHECK: 1,2,3
f(#variadic([1,2,3])) // CHECK: 1,2,3
closure(#variadic([1,2,3])) // CHECK: 1,2,3
let x = [1,2,3].map { $0 + 1 }
f(#variadic(x)) // CHECK: 2,3,4
f(#variadic([x, x])) // CHECK: [2, 3, 4],[2, 3, 4]

func overloaded(x: [Int]) { print("incorrect") }
func overloaded(x: Int...) { print("correct") }
overloaded(x: #variadic(x)) // CHECK: correct

struct HasVariadicSubscript {
  subscript(bar: Int...) -> Int {
    get {
      print(#variadic(bar), separator: ",")
      return 42
    }
  }
}
_ = HasVariadicSubscript()[9,8,7] // CHECK: 9,8,7

func triangle(_ x: Int...) {
  if x.isEmpty {
    return
  }
  print(#variadic(x), separator: ",")
  triangle(#variadic(x.dropLast()))
}

triangle(1,2,3,4,5)
// CHECK: 1,2,3,4,5
// CHECK: 1,2,3,4
// CHECK: 1,2,3
// CHECK: 1,2
// CHECK: 1

