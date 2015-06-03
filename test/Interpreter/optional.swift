// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

class A {
  func printA() { print("A", appendNewline: false) }
}
class B : A {
  override func printA() { print("B", appendNewline: false) }
}

func printA(v: A) { v.printA() }
func printOpt<T>(subprint: T->())(x: T?) {
  switch (x) {
  case .Some(let y): print(".Some(", appendNewline: false); subprint(y); print(")", appendNewline: false)
  case .None: print(".None", appendNewline: false)
  }
}

func test(v: A????, _ cast: (A????) -> B?) {
  printOpt(printOpt(printOpt(printOpt(printA))))(x: v)
  print(" as? B: ", appendNewline: false)
  printOpt(printA)(x: cast(v))
  print("\n", appendNewline: false)
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
  print(" as? B?: ", appendNewline: false)
  printOpt(printOpt(printA))(x: cast(v))
  print("\n", appendNewline: false)
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
  print(" as? B??: ", appendNewline: false)
  printOpt(printOpt(printOpt(printA)))(x: cast(v))
  print("\n", appendNewline: false)
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
  case .Some(nil): print("y is .Some(nil)")
  case .Some(let v): print("y is .Some(\(v))")
}
// CHECK: y is .Some(nil)

switch z {
  case nil:  print("z is nil")
  case .Some(nil): print("z is .Some(nil)")
  case .Some(let v): print("z is .Some(\(v))")
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
