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
  print(x as Any..., separator: ",")
}

func f(_ x: [Int]...) {
  print(x as Any..., separator: ",")
}

let closure: ((Int...) -> Void) = {
  (args: Int...) in
  print(args as Any..., separator: ",")
}

f(1,2,3) // CHECK: 1,2,3
f([1,2,3] as Int...) // CHECK: 1,2,3
closure([1,2,3] as Int...) // CHECK: 1,2,3
let x = [1,2,3].map { $0 + 1 }
f(x as Int...) // CHECK: 2,3,4
f([x, x] as [Int]...) // CHECK: [2, 3, 4],[2, 3, 4]

func overloaded(x: [Int]) { print("array") }
func overloaded(x: Int...) { print("varargs") }
overloaded(x: x as Int...) // CHECK: varargs
overloaded(x: x) // CHECK: array

struct HasVariadicSubscript {
  subscript(bar: Int...) -> Int {
    get {
      print(bar as Any..., separator: ",")
      return 42
    }
  }
}
_ = HasVariadicSubscript()[[9,8,7] as Int...] // CHECK: 9,8,7

func triangle(_ x: Int...) {
  if x.isEmpty {
    return
  }
  print(x as Any..., separator: ",")
  triangle(x.dropLast() as Int...)
}

triangle(1,2,3,4,5)
// CHECK: 1,2,3,4,5
// CHECK: 1,2,3,4
// CHECK: 1,2,3
// CHECK: 1,2
// CHECK: 1

