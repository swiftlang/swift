// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

class A {
  func printA() { print("A", terminator: "") }
}
class B : A {
  override func printA() { print("B", terminator: "") }
}

func printA(_ v: A) { v.printA() }
func printOpt<T>(_ subprint: @escaping (T) -> ()) -> (T?) -> () {
  return { x in
    switch (x) {
    case .some(let y): print(".some(", terminator: ""); subprint(y); print(")", terminator: "")
    case .none: print(".none", terminator: "")
    }
  }
}

func test(_ v: A????, _ cast: (A????) -> B?) {
  printOpt(printOpt(printOpt(printOpt(printA))))(v)
  print(" as? B: ", terminator: "")
  printOpt(printA)(cast(v))
  print("\n", terminator: "")
}
test(.some(.some(.some(.some(A())))), { $0 as? B })
test(.some(.some(.some(.some(B())))), { $0 as? B })
test(.some(.some(.some(.none))), { $0 as? B })
test(.some(.some(.none)), { $0 as? B })
test(.some(.none), { $0 as? B })
test(.none, { $0 as? B })
// CHECK: .some(.some(.some(.some(A)))) as? B: .none
// CHECK: .some(.some(.some(.some(B)))) as? B: .some(B)
// CHECK: .some(.some(.some(.none))) as? B: .none
// CHECK: .some(.some(.none)) as? B: .none
// CHECK: .some(.none) as? B: .none
// CHECK: .none as? B: .none

func test(_ v: A????, _ cast: (A????) -> B??) {
  printOpt(printOpt(printOpt(printOpt(printA))))(v)
  print(" as? B?: ", terminator: "")
  printOpt(printOpt(printA))(cast(v))
  print("\n", terminator: "")
}
test(.some(.some(.some(.some(A())))), { $0 as? B? })
test(.some(.some(.some(.some(B())))), { $0 as? B? })
test(.some(.some(.some(.none))), { $0 as? B? })
test(.some(.some(.none)), { $0 as? B? })
test(.some(.none), { $0 as? B? })
test(.none, { $0 as? B? })
// CHECK: .some(.some(.some(.some(A)))) as? B?: .none
// CHECK: .some(.some(.some(.some(B)))) as? B?: .some(.some(B))
// CHECK: .some(.some(.some(.none))) as? B?: .some(.none)
// CHECK: .some(.some(.none)) as? B?: .none
// CHECK: .some(.none) as? B?: .none
// CHECK: .none as? B?: .none

func test(_ v: A????, _ cast: (A????) -> B???) {
  printOpt(printOpt(printOpt(printOpt(printA))))(v)
  print(" as? B??: ", terminator: "")
  printOpt(printOpt(printOpt(printA)))(cast(v))
  print("\n", terminator: "")
}
test(.some(.some(.some(.some(A())))), { $0 as? B?? })
test(.some(.some(.some(.some(B())))), { $0 as? B?? })
test(.some(.some(.some(.none))), { $0 as? B?? })
test(.some(.some(.none)), { $0 as? B?? })
test(.some(.none), { $0 as? B?? })
test(.none, { $0 as? B?? })
// CHECK: .some(.some(.some(.some(A)))) as? B??: .none
// CHECK: .some(.some(.some(.some(B)))) as? B??: .some(.some(.some(B)))
// CHECK: .some(.some(.some(.none))) as? B??: .some(.some(.none))
// CHECK: .some(.some(.none)) as? B??: .some(.none)
// CHECK: .some(.none) as? B??: .none
// CHECK: .none as? B??: .none

class Foo : Equatable {
}
func ==(a : Foo, b : Foo) -> Bool { return a === b }


var x_foo: Foo! = nil
if x_foo == nil { print("x_foo is nil") }
// CHECK: x_foo is nil
if x_foo != nil { print("x_foo is not nil") } else { print("x_foo is nil") }
// CHECK: x_foo is nil
if nil == x_foo { print("x_foo is nil") }
// CHECK: x_foo is nil
if nil != x_foo { print("x_foo is not nil") } else { print("x_foo is nil") }
// CHECK: x_foo is nil

var y_foo: Foo? = nil
if y_foo == nil { print("y_foo is nil") }
// CHECK: y_foo is nil
if y_foo != nil { print("y_foo is not nil") } else { print("y_foo is nil") }
// CHECK: y_foo is nil
if nil == y_foo { print("y_foo is nil") }
// CHECK: y_foo is nil
if nil != y_foo { print("y_foo is not nil") } else { print("y_foo is nil") }
// CHECK: y_foo is nil

var x : Int? = nil
var y : Int?? = x
var z : Int?? = nil

switch y {
  case nil:  print("y is nil")
  case .some(nil): print("y is .some(nil)")
  case .some(let v): print("y is .some(\(v))")
}
// CHECK: y is .some(nil)

switch z {
  case nil:  print("z is nil")
  case .some(nil): print("z is .some(nil)")
  case .some(let v): print("z is .some(\(v))")
}
// CHECK: z is nil

// Validate nil equality comparisons with non-equatable optional types
class C {}
var c: C? = nil

print(c == nil)
// CHECK: true

print(nil == c)
// CHECK: true

print(c != nil)
// CHECK: false

print(nil != c)
// CHECK: false

var c2: C? = C()

print(c2 == nil)
// CHECK: false

print(nil == c2)
// CHECK: false

print(c2 != nil)
// CHECK: true

print(nil != c2)
// CHECK: true

var c3: C! = nil

print(c3 == nil)
// CHECK: true

print(nil == c3)
// CHECK: true

print(c3 != nil)
// CHECK: false

print(nil != c3)
// CHECK: false

var c4: C! = C()

print(c4 == nil)
// CHECK: false

print(nil == c4)
// CHECK: false

print(c4 != nil)
// CHECK: true

print(nil != c4)
// CHECK: true
