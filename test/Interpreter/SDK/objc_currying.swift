// RUN: %target-run-simple-swift | FileCheck %s
import Foundation

func foo(f: String! -> String!) {
  println(f(", "))
  println(f(" • "))
}

let x: NSArray = ["foo", "bar", "bas"]
foo(x.componentsJoinedByString)
// CHECK: foo, bar, bas
// CHECK: foo • bar • bas

// TODO
// let a: AnyObject = x
// if let componentsJoinedByString = a.componentsJoinedByString {
//   foo(componentsJoinedByString)
// }
