// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

// LValues are direct values, too. They are reference types, though.

func markUsed<T>(_ t: T) {}

class Class {
  var ivar : Int64
  init() { ivar = 1234 }
}

class Other : Class {
  var ovar : Int64
  override init() {
    ovar = 112233
    super.init()
    ivar = 4321
  }
}

struct Struct {
  var ivar : Int64
  init() { ivar = 4567 }
}

func foo(_ x: inout Class) {
// CHECK: !DILocalVariable(name: "x", arg: 1{{.*}} line: [[@LINE-1]]
  markUsed(x.ivar)
  x.ivar += 1 // Set breakpoint here
}

func foo(_ x: inout Struct) {
// CHECK: !DILocalVariable(name: "x", arg: 1{{.*}} line: [[@LINE-1]]
  markUsed(x.ivar)
  x.ivar += 1 // Set breakpoint here
}

func main() {
  var c: Class = Other()
  var s = Struct()
  foo(&c)
  foo(&s)
  foo(&c)
  foo(&s)
}

main()

