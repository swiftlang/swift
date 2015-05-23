// RUN: %target-parse-verify-swift

func f0(inout x: Int) {}
func f1<T>(inout x: T) {}
func f2(inout x: X) {}
func f2(inout x: Double) {}

class Reftype {
  var property: Double { get {} set {} }
}

struct X {
  subscript(i: Int) -> Float { get {} set {} }

  var property: Double { get {} set {} }

  func genuflect() {}
}

struct Y {
  subscript(i: Int) -> Float { get {} set {} }
  subscript(f: Float) -> Int { get {} set {} }
}

var i : Int
var f : Float
var x : X
var y : Y

func +=(inout lhs: X, rhs : X) {}
func +=(inout lhs: Double, rhs : Double) {}
prefix func ++(inout rhs: X) {}
postfix func ++(inout lhs: X) {}

f0(&i)
f1(&i)
f1(&x[i])
f1(&x.property)
f1(&y[i])

// Missing '&'
f0(i) // expected-error{{passing value of type 'Int' to an inout parameter requires explicit '&'}}{{4-4=&}}
f1(y[i]) // expected-error{{passing value of type 'Float' to an inout parameter requires explicit '&'}}

// Assignment operators
x += x
++x

var yi = y[i]

// Non-settable lvalues
// FIXME: better diagnostic!

var non_settable_x : X {
  return x
}

struct Z {
  var non_settable_x: X { get {} }
  var non_settable_reftype: Reftype { get {} }
  var settable_x : X

  subscript(i: Int) -> Double { get {} }
  subscript((i: Int, j: Int)) -> X { get {} }
}

var z : Z

func fz() -> Z {}
func fref() -> Reftype {}

// non-settable var is non-settable:
// - assignment
non_settable_x = x // expected-error{{cannot assign to a get-only property 'non_settable_x'}}
// - inout (mono)
f2(&non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
// - inout (generic)
f1(&non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
// - inout assignment
non_settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
++non_settable_x // expected-error{{cannot pass immutable value to mutating operator: 'non_settable_x' is a get-only property}}

// non-settable property is non-settable:
z.non_settable_x = x // expected-error{{cannot assign to a get-only property 'non_settable_x'}}
f2(&z.non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
f1(&z.non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
z.non_settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
++z.non_settable_x // expected-error{{cannot pass immutable value to mutating operator: 'non_settable_x' is a get-only property}}

// non-settable subscript is non-settable:
z[0] = 0.0 // expected-error{{cannot assign through subscript: 'z' is immutable}}
f2(&z[0]) // expected-error{{cannot pass immutable value as inout argument: 'z' is immutable}}
f1(&z[0]) // expected-error{{cannot pass immutable value as inout argument: 'z' is immutable}}
z[0] += 0.0 // expected-error{{cannot pass immutable value of type 'Double' to mutating binary operator '+='}}
++z[0] // expected-error{{cannot pass immutable value to mutating operator: 'z' is immutable}}

// settable property of an rvalue value type is non-settable:
fz().settable_x = x // expected-error{{cannot assign to 'settable_x', base has immutable type 'Z'}}
f2(&fz().settable_x) // expected-error{{cannot pass immutable value as inout argument: 'fz' returns immutable value}}
f1(&fz().settable_x) // expected-error{{cannot pass immutable value as inout argument: 'fz' returns immutable value}}
fz().settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'fz' returns immutable value}}
++fz().settable_x // expected-error{{cannot pass immutable value to mutating operator: 'fz' returns immutable value}}

// settable property of an rvalue reference type IS SETTABLE:
fref().property = 0.0
f2(&fref().property)
f1(&fref().property)
fref().property += 0.0
++fref().property

// settable property of a non-settable value type is non-settable:
z.non_settable_x.property = 1.0 // expected-error{{cannot assign to 'property': 'non_settable_x' is immutable}}
f2(&z.non_settable_x.property) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
f1(&z.non_settable_x.property) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
z.non_settable_x.property += 1.0 // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
++z.non_settable_x.property // expected-error{{cannot pass immutable value to mutating operator: 'non_settable_x' is a get-only property}}

// settable property of a non-settable reference type IS SETTABLE:
z.non_settable_reftype.property = 1.0
f2(&z.non_settable_reftype.property)
f1(&z.non_settable_reftype.property)
z.non_settable_reftype.property += 1.0
++z.non_settable_reftype.property

// regressions with non-settable subscripts in value contexts
z[0] == 0
var d : Double
d = z[0]

// regressions with subscripts that return generic types
var xs:[X]
_ = xs[0].property

struct A<T> {
    subscript(i: Int) -> T { get {} }
}

struct B {
    subscript(i: Int) -> Int { get {} }
}

var a:A<B>

_ = a[0][0]

// Instance members of struct metatypes.
struct FooStruct {
  func instanceFunc0() {}
}

func testFooStruct() {
  FooStruct.instanceFunc0(FooStruct())()
}

// Don't load from explicit lvalues.
func takesInt(x: Int) {}
func testInOut(inout arg: Int) {
  var x : Int
  takesInt(&x) // expected-error{{cannot invoke 'takesInt' with an argument list of type '(inout Int)'}} expected-note{{expected an argument list of type '(Int)'}}
}

// Don't infer inout types.
var ir = &i // expected-error{{type 'inout Int' of variable is not materializable}} \
            // expected-error{{reference to 'Int' not used to initialize a inout parameter}}
var ir2 = ((&i)) // expected-error{{type 'inout Int' of variable is not materializable}} \
                 // expected-error{{reference to 'Int' not used to initialize a inout parameter}}

// <rdar://problem/17133089>
func takeArrayRef(inout x:Array<String>) { }

// FIXME: Poor diagnostic.
takeArrayRef(["asdf", "1234"]) // expected-error{{cannot invoke 'takeArrayRef' with an argument list of type '([String])'}} expected-note{{expected an argument list of type '(inout Array<String>)'}}

// <rdar://problem/19835413> Reference to value from array changed
func rdar19835413() {
  func f1(p: UnsafeMutablePointer<Void>) {}
  func f2(var a: [Int], i: Int, pi: UnsafeMutablePointer<Int>) {
    f1(&a)
    f1(&a[i])
    f1(&a[0])
    f1(pi)
    f1(UnsafeMutablePointer(pi))
  }
}
