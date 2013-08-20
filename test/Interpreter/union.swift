// RUN: %swift -i %s | FileCheck %s

union Singleton {
  case x(Int, Char)
}

union NoPayload {
  case x
  case y
  case z
}

union SinglePayloadTrivial {
  case x(Char, Int)
  case y
  case z
}

var s = Singleton.x(1, 'a')
switch s {
case .x(var int, var char):
  // CHECK: 1
  println(int)
  // CHECK: a
  println(char)
}

func printNoPayload(v:NoPayload) {
  switch v {
  case .x:
    println("NoPayload.x")
  case .y:
    println("NoPayload.y")
  case .z:
    println("NoPayload.z")
  }
}

// CHECK: NoPayload.x
printNoPayload(.x)
// CHECK: NoPayload.y
printNoPayload(.y)
// CHECK: NoPayload.z
printNoPayload(.z)

func printSinglePayloadTrivial(v:SinglePayloadTrivial) {
  switch v {
  case .x(var char, var int):
    println("SinglePayloadTrivial.x(\(char), \(int))")
  case .y:
    println("SinglePayloadTrivial.y")
  case .z:
    println("SinglePayloadTrivial.z")
  }
}

printSinglePayloadTrivial(.x('b', 2))
printSinglePayloadTrivial(.y)
printSinglePayloadTrivial(.z)
