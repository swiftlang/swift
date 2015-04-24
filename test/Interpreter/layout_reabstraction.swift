// RUN: %target-run-simple-swift | FileCheck %s

struct S {}
struct Q {}

func printMetatype<T>(x: Any, _: T.Type) {
  println(x as! T.Type)
}

func printMetatypeConditional<T>(x: Any, _: T.Type) {
  if let y = x as? T.Type {
    println(y)
  } else {
    println("nope")
  }
}

var any: Any = S.self
// CHECK: downcast in substituted context:
println("downcast in substituted context:")
// CHECK-NEXT: main.S
println(any as! S.Type)
// CHECK-NEXT: nope
if let q = any as? Q.Type {
  println(q)
} else {
  println("nope")
}

// CHECK-NEXT: downcast in generic context:
println("downcast in generic context:")
// CHECK-NEXT: main.S
printMetatype(any, S.self)
// CHECK-NEXT: main.S
printMetatypeConditional(any, S.self)
// CHECK-NEXT: nope
printMetatypeConditional(any, Q.self)

// Unspecialized wrapper around sizeof(T) to force us to get the runtime's idea
// of the size of a type.
@inline(never)
func unspecializedSizeOf<T>(t: T.Type) -> Int {
  return sizeof(t)
}

struct ContainsTrivialMetatype<T> {
  var x: T
  var meta: S.Type
}

struct ContainsTupleOfTrivialMetatype<T> {
  var x: (T, S.Type)
}

// CHECK-NEXT: 8
println(sizeof(ContainsTrivialMetatype<Int64>.self))
// CHECK-NEXT: 8
println(unspecializedSizeOf(ContainsTrivialMetatype<Int64>.self))

// CHECK-NEXT: 8
println(sizeof(ContainsTupleOfTrivialMetatype<Int64>.self))
// CHECK-NEXT: 8
println(unspecializedSizeOf(ContainsTupleOfTrivialMetatype<Int64>.self))

struct ContainsTupleOfFunctions<T> {
  var x: (T, T -> T)
  
  func apply() -> T {
    return x.1(x.0)
  }
}

// CHECK-NEXT: 2
println(sizeof(ContainsTupleOfFunctions<()>.self) / sizeof(Int.self))
// CHECK-NEXT: 2
println(unspecializedSizeOf(ContainsTupleOfFunctions<()>.self) / sizeof(Int.self))
// CHECK-NEXT: 3
println(sizeof(ContainsTupleOfFunctions<Int>.self) / sizeof(Int.self))
// CHECK-NEXT: 3
println(unspecializedSizeOf(ContainsTupleOfFunctions<Int>.self) / sizeof(Int.self))

let x = ContainsTupleOfFunctions(x: (1, { $0 + 1 }))
let y = ContainsTupleOfFunctions(x: ("foo", { $0 + "bar" }))

// CHECK-NEXT: 2
println(x.apply())
// CHECK-NEXT: foobar
println(y.apply())

func callAny<T>(f: Any, _ x: T) -> T {
  return (f as! T -> T)(x)
}

any = {(x: Int) -> Int in x + x}
// CHECK-NEXT: 24
println((any as! Int -> Int)(12))
// CHECK-NEXT: 24
println(callAny(any, 12))
