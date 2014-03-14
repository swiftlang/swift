// RUN: %target-run-simple-swift | FileCheck %s

import SwiftShims

class Foo : _ObjCSuperClassHack {
  var x : Int = 123
  func bar() {}
}
println("hello") // CHECK: hello
