// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-source-import

import imported_enums

// TODO: Implement tuple equality in the library.
// BLOCKED: <rdar://problem/13822406>
func ~= (x: (Int,Int,Int), y: (Int,Int,Int)) -> Bool {
  return true
}

var x:Int

func square(_ x: Int) -> Int { return x*x }

struct A<B> {
  struct C<D> { }
}

switch x {
// Expressions as patterns.
case 0:
  ()
case 1 + 2:
  ()
case square(9):
  ()

// 'var' and 'let' patterns.
case var a:
  a = 1
case let a:
  a = 1         // expected-error {{cannot assign}}
case var var a: // expected-error {{'var' cannot appear nested inside another 'var' or 'let' pattern}}
  a += 1
case var let a: // expected-error {{'let' cannot appear nested inside another 'var' or 'let' pattern}}
  print(a, terminator: "")
case var (var b): // expected-error {{'var' cannot appear nested inside another 'var'}}
  b += 1

// 'Any' pattern.
case _:
  ()

// patterns are resolved in expression-only positions are errors.
case 1 + (_): // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}
  ()
}

switch (x,x) {
case (var a, var a): // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'a' is here}} expected-warning {{variable 'a' was never used; consider replacing with '_' or removing it}} expected-warning {{variable 'a' was never used; consider replacing with '_' or removing it}}
  fallthrough
case _:
  ()
}

var e : Any = 0

switch e {
// 'is' pattern.
case is Int,
     is A<Int>,
     is A<Int>.C<Int>,
     is (Int, Int),
     is (a: Int, b: Int):
  ()
}

// Enum patterns.
enum Foo { case A, B, C }

func == <T>(_: Voluntary<T>, _: Voluntary<T>) -> Bool { return true }

enum Voluntary<T> : Equatable {
  case Naught
  case Mere(T)
  case Twain(T, T)


  func enumMethod(_ other: Voluntary<T>, foo: Foo) {
    switch self {
    case other:
      ()

    case .Naught,
         .Naught(),
         .Naught(_, _): // expected-error{{tuple pattern has the wrong length for tuple type '()'}}
      ()

    case .Mere,
         .Mere(), // expected-error{{tuple pattern cannot match values of the non-tuple type 'T'}}
         .Mere(_),
         .Mere(_, _): // expected-error{{tuple pattern cannot match values of the non-tuple type 'T'}}
      ()

    case .Twain(), // expected-error{{tuple pattern has the wrong length for tuple type '(T, T)'}}
         .Twain(_),
         .Twain(_, _),
         .Twain(_, _, _): // expected-error{{tuple pattern has the wrong length for tuple type '(T, T)'}}
      ()
    }

    switch foo {
    case .Naught: // expected-error{{enum case 'Naught' not found in type 'Foo'}}
      ()
    case .A, .B, .C:
      ()
    }
  }
}

var n : Voluntary<Int> = .Naught

switch n {
case Foo.A: // expected-error{{enum case 'A' is not a member of type 'Voluntary<Int>'}}
  ()
case Voluntary<Int>.Naught,
     Voluntary<Int>.Naught(),
     Voluntary<Int>.Naught(_, _), // expected-error{{tuple pattern has the wrong length for tuple type '()'}}
     Voluntary.Naught,
     .Naught:
  ()
case Voluntary<Int>.Mere,
     Voluntary<Int>.Mere(_),
     Voluntary<Int>.Mere(_, _), // expected-error{{tuple pattern cannot match values of the non-tuple type 'Int'}}
     Voluntary.Mere,
     Voluntary.Mere(_),
     .Mere,
     .Mere(_):
  ()
case .Twain,
     .Twain(_),
     .Twain(_, _),
     .Twain(_, _, _): // expected-error{{tuple pattern has the wrong length for tuple type '(Int, Int)'}}
  ()
}

var notAnEnum = 0

switch notAnEnum {
case .Foo: // expected-error{{enum case 'Foo' not found in type 'Int'}}
  ()
}

struct ContainsEnum {
  enum Possible<T> {
    case Naught
    case Mere(T)
    case Twain(T, T)
  }

  func member(_ n: Possible<Int>) {
    switch n {
    case ContainsEnum.Possible<Int>.Naught,
         ContainsEnum.Possible.Naught,
         Possible<Int>.Naught,
         Possible.Naught,
         .Naught:
      ()
    }
  }
}

func nonmemberAccessesMemberType(_ n: ContainsEnum.Possible<Int>) {
  switch n {
  case ContainsEnum.Possible<Int>.Naught,
       .Naught:
    ()
  }
}

var m : ImportedEnum = .Simple

switch m {
case imported_enums.ImportedEnum.Simple,
     ImportedEnum.Simple,
     .Simple:
  ()
case imported_enums.ImportedEnum.Compound,
     imported_enums.ImportedEnum.Compound(_),
     ImportedEnum.Compound,
     ImportedEnum.Compound(_),
     .Compound,
     .Compound(_):
  ()
}

// Check that single-element tuple payloads work sensibly in patterns.

enum LabeledScalarPayload {
  case Payload(name: Int)
}

var lsp: LabeledScalarPayload = .Payload(name: 0)
func acceptInt(_: Int) {}
func acceptString(_: String) {}

switch lsp {
case .Payload(0):
  ()
case .Payload(name: 0):
  ()
case let .Payload(x):
  acceptInt(x)
  acceptString("\(x)")
case let .Payload(name: x):
  acceptInt(x)
  acceptString("\(x)")
case let .Payload((name: x)):
  acceptInt(x)
  acceptString("\(x)")
case .Payload(let (name: x)):
  acceptInt(x)
  acceptString("\(x)")
case .Payload(let (name: x)):
  acceptInt(x)
  acceptString("\(x)")
case .Payload(let x):
  acceptInt(x)
  acceptString("\(x)")
case .Payload((let x)):
  acceptInt(x)
  acceptString("\(x)")
}

// Property patterns.

struct S {
  static var stat: Int = 0
  var x, y : Int
  var comp : Int {
    return x + y
  }

  func nonProperty() {}
}





// Tuple patterns.

var t = (1, 2, 3)

prefix operator +++
infix operator +++
prefix func +++(x: (Int,Int,Int)) -> (Int,Int,Int) { return x }
func +++(x: (Int,Int,Int), y: (Int,Int,Int)) -> (Int,Int,Int) {
  return (x.0+y.0, x.1+y.1, x.2+y.2)
}

switch t {
case (_, var a, 3):
  a += 1
case var (_, b, 3):
  b += 1
case var (_, var c, 3): // expected-error{{'var' cannot appear nested inside another 'var'}}
  c += 1
case (1, 2, 3):
  ()

// patterns in expression-only positions are errors.
case +++(_, var d, 3):
// expected-error@-1{{'+++' is not a prefix unary operator}}
  ()
case (_, var e, 3) +++ (1, 2, 3):
// expected-error@-1{{binary operator '+++' cannot be applied to operands of type '(_, <<error type>>, Int)' and '(Int, Int, Int)'}}
// expected-note@-2{{expected an argument list of type '((Int, Int, Int), (Int, Int, Int))'}}
// expected-error@-3{{'var' binding pattern cannot appear in an expression}}
// expected-error@-4{{'var' binding pattern cannot appear in an expression}}
  ()
}

// FIXME: We don't currently allow subpatterns for "isa" patterns that
// require interesting conditional downcasts.
class Base { }
class Derived : Base { }


switch [Derived(), Derived(), Base()] {
case let ds as [Derived]: // expected-error{{downcast pattern value of type '[Derived]' cannot be used}}
  ()

default:
  ()
}


// Optional patterns.
let op1 : Int?
let op2 : Int??

switch op1 {
case nil: break
case 1?: break
case _?: break
}

switch op2 {
case nil: break
case _?: break
case (1?)?: break
case (_?)?: break
}



// <rdar://problem/20365753> Bogus diagnostic "refutable pattern match can fail"
let (responseObject: Int?) = op1
// expected-error @-1 {{expected ',' separator}} {{25-25=,}}
// expected-error @-2 {{expected pattern}}


