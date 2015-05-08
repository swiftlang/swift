// RUN: %target-run-simple-swift | FileCheck %s

struct S {}
struct Q {}

func printMetatype<T>(x: Any, _: T.Type) {
  print(x as! T.Type)
}

func printMetatypeConditional<T>(x: Any, _: T.Type) {
  if let y = x as? T.Type {
    print(y)
  } else {
    print("nope")
  }
}

var any: Any = S.self
// CHECK: downcast in substituted context:
print("downcast in substituted context:")
// CHECK-NEXT: main.S
print(any as! S.Type)
// CHECK-NEXT: nope
if let q = any as? Q.Type {
  print(q)
} else {
  print("nope")
}

// CHECK-NEXT: downcast in generic context:
print("downcast in generic context:")
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
print(sizeof(ContainsTrivialMetatype<Int64>.self))
// CHECK-NEXT: 8
print(unspecializedSizeOf(ContainsTrivialMetatype<Int64>.self))

// CHECK-NEXT: 8
print(sizeof(ContainsTupleOfTrivialMetatype<Int64>.self))
// CHECK-NEXT: 8
print(unspecializedSizeOf(ContainsTupleOfTrivialMetatype<Int64>.self))

struct ContainsTupleOfFunctions<T> {
  var x: (T, T -> T)
  
  func apply() -> T {
    return x.1(x.0)
  }
}

// CHECK-NEXT: 2
print(sizeof(ContainsTupleOfFunctions<()>.self) / sizeof(Int.self))
// CHECK-NEXT: 2
print(unspecializedSizeOf(ContainsTupleOfFunctions<()>.self) / sizeof(Int.self))
// CHECK-NEXT: 3
print(sizeof(ContainsTupleOfFunctions<Int>.self) / sizeof(Int.self))
// CHECK-NEXT: 3
print(unspecializedSizeOf(ContainsTupleOfFunctions<Int>.self) / sizeof(Int.self))

let x = ContainsTupleOfFunctions(x: (1, { $0 + 1 }))
let y = ContainsTupleOfFunctions(x: ("foo", { $0 + "bar" }))

// CHECK-NEXT: 2
print(x.apply())
// CHECK-NEXT: foobar
print(y.apply())

func callAny<T>(f: Any, _ x: T) -> T {
  return (f as! T -> T)(x)
}

any = {(x: Int) -> Int in x + x}
// CHECK-NEXT: 24
print((any as! Int -> Int)(12))
// CHECK-NEXT: 24
print(callAny(any, 12))
