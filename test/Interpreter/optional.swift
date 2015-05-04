// RUN: %target-run-simple-swift | FileCheck %s

class A {
  func printA() { print("A") }
}
class B : A {
  override func printA() { print("B") }
}

func printA(v: A) { v.printA() }
func printOpt<T>(subprint: T->())(x: T?) {
  switch (x) {
  case .Some(let y): print(".Some("); subprint(y); print(")")
  case .None: print(".None")
  }
}

func test(v: A????, _ cast: (A????) -> B?) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as? B: ")
  printOpt(printA)(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as? B })
test(.Some(.Some(.Some(.Some(B())))), { $0 as? B })
test(.Some(.Some(.Some(.None))), { $0 as? B })
test(.Some(.Some(.None)), { $0 as? B })
test(.Some(.None), { $0 as? B })
test(.None, { $0 as? B })
// CHECK: .Some(.Some(.Some(.Some(A)))) as? B: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as? B: .Some(B)
// CHECK: .Some(.Some(.Some(.None))) as? B: .None
// CHECK: .Some(.Some(.None)) as? B: .None
// CHECK: .Some(.None) as? B: .None
// CHECK: .None as? B: .None

func test(v: A????, _ cast: (A????) -> B??) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as? B?: ")
  printOpt(printOpt(printA))(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as? B? })
test(.Some(.Some(.Some(.Some(B())))), { $0 as? B? })
test(.Some(.Some(.Some(.None))), { $0 as? B? })
test(.Some(.Some(.None)), { $0 as? B? })
test(.Some(.None), { $0 as? B? })
test(.None, { $0 as? B? })
// CHECK: .Some(.Some(.Some(.Some(A)))) as? B?: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as? B?: .Some(.Some(B))
// CHECK: .Some(.Some(.Some(.None))) as? B?: .Some(.None)
// CHECK: .Some(.Some(.None)) as? B?: .None
// CHECK: .Some(.None) as? B?: .None
// CHECK: .None as? B?: .None

func test(v: A????, _ cast: (A????) -> B???) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as? B??: ")
  printOpt(printOpt(printOpt(printA)))(x: cast(v))
  print("\n")
}
test(.Some(.Some(.Some(.Some(A())))), { $0 as? B?? })
test(.Some(.Some(.Some(.Some(B())))), { $0 as? B?? })
test(.Some(.Some(.Some(.None))), { $0 as? B?? })
test(.Some(.Some(.None)), { $0 as? B?? })
test(.Some(.None), { $0 as? B?? })
test(.None, { $0 as? B?? })
// CHECK: .Some(.Some(.Some(.Some(A)))) as? B??: .None
// CHECK: .Some(.Some(.Some(.Some(B)))) as? B??: .Some(.Some(.Some(B)))
// CHECK: .Some(.Some(.Some(.None))) as? B??: .Some(.Some(.None))
// CHECK: .Some(.Some(.None)) as? B??: .Some(.None)
// CHECK: .Some(.None) as? B??: .None
// CHECK: .None as? B??: .None

class Foo : Equatable {
}
func ==(a : Foo, b : Foo) -> Bool { return a === b }


var x_foo: Foo! = nil
if x_foo == nil { println("x_foo is nil") }
// CHECK: x_foo is nil
if x_foo != nil { println("x_foo is not nil") } else { println("x_foo is nil") }
// CHECK: x_foo is nil
if nil == x_foo { println("x_foo is nil") }
// CHECK: x_foo is nil
if nil != x_foo { println("x_foo is not nil") } else { println("x_foo is nil") }
// CHECK: x_foo is nil

var y_foo: Foo? = nil
if y_foo == nil { println("y_foo is nil") }
// CHECK: y_foo is nil
if y_foo != nil { println("y_foo is not nil") } else { println("y_foo is nil") }
// CHECK: y_foo is nil
if nil == y_foo { println("y_foo is nil") }
// CHECK: y_foo is nil
if nil != y_foo { println("y_foo is not nil") } else { println("y_foo is nil") }
// CHECK: y_foo is nil

var x : Int? = nil
var y : Int?? = x
var z : Int?? = nil

switch y {
  case nil:  println("y is nil")
  case .Some(nil): println("y is .Some(nil)")
  case .Some(let v): println("y is .Some(\(v))")
}
// CHECK: y is .Some(nil)

switch z {
  case nil:  println("z is nil")
  case .Some(nil): println("z is .Some(nil)")
  case .Some(let v): println("z is .Some(\(v))")
}
// CHECK: z is nil

// Validate nil equality comparisons with non-equatable optional types
class C {}
var c: C? = nil

println(c == nil)
// CHECK: true

println(nil == c)
// CHECK: true

println(c != nil)
// CHECK: false

println(nil != c)
// CHECK: false

var c2: C? = C()

println(c2 == nil)
// CHECK: false

println(nil == c2)
// CHECK: false

println(c2 != nil)
// CHECK: true

println(nil != c2)
// CHECK: true

var c3: C! = nil

println(c3 == nil)
// CHECK: true

println(nil == c3)
// CHECK: true

println(c3 != nil)
// CHECK: false

println(nil != c3)
// CHECK: false

var c4: C! = C()

println(c4 == nil)
// CHECK: false

println(nil == c4)
// CHECK: false

println(c4 != nil)
// CHECK: true

println(nil != c4)
// CHECK: true
