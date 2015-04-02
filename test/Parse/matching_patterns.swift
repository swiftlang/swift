// RUN: %target-parse-verify-swift -enable-experimental-patterns -I %S/Inputs -enable-source-import

import imported_enums

// TODO: Implement tuple equality in the library.
// BLOCKED: <rdar://problem/13822406>
func ~= (x: (Int,Int,Int), y: (Int,Int,Int)) -> Bool {
  return true
}

var x:Int

func square(x: Int) -> Int { return x*x }

struct A<B> {
  struct C<D> { } // expected-error{{generic type 'C' nested in type}}
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
  print(a)
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
case (var a, var a): // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'a' is here}}
  fallthrough
case _:
  ()
}


var e : protocol<> = 0

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


  func enumMethod(other: Voluntary<T>, foo: Foo) {
    switch self {
    case other:
      ()

    case Naught,
         Naught(),
         Naught(_, _): // expected-error{{tuple pattern has the wrong length for tuple type '()'}}
      ()

    case Mere,
         Mere(), // expected-error{{tuple pattern cannot match values of the non-tuple type 'T'}}
         Mere(_),
         Mere(_, _): // expected-error{{tuple pattern cannot match values of the non-tuple type 'T'}}
      ()

    case Twain(), // expected-error{{tuple pattern has the wrong length for tuple type '(T, T)'}}
         Twain(_),
         Twain(_, _),
         Twain(_, _, _): // expected-error{{tuple pattern has the wrong length for tuple type '(T, T)'}}
      ()
    }

    switch foo {
    case Naught: // expected-error{{enum case 'Naught' is not a member of type 'Foo'}}
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
  enum Possible<T> { // expected-error{{generic type 'Possible' nested in type}}
    case Naught
    case Mere(T)
    case Twain(T, T)
  }

  func member(n: Possible<Int>) {
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

func nonmemberAccessesMemberType(n: ContainsEnum.Possible<Int>) {
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
case let .Payload((name: x)): // expected-error {{label is not allowed on single element tuple pattern}} expected-note {{remove the parentheses to make this a type annotation}} expected-note {{remove the label to make this a tuple pattern}}
  acceptInt(x)
  acceptString("\(x)")
case .Payload(let (name: x)): // expected-error {{label is not allowed on single element tuple pattern}} expected-note {{remove the parentheses to make this a type annotation}} expected-note {{remove the label to make this a tuple pattern}}
  acceptInt(x)
  acceptString("\(x)")
case .Payload(let (name: x)): // expected-error {{label is not allowed on single element tuple pattern}} expected-note {{remove the parentheses to make this a type annotation}} expected-note {{remove the label to make this a tuple pattern}}
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

struct T {}

var s: S

switch s {
case S():
  ()
case S(stat: _): // expected-error{{cannot match type property 'stat' of type 'S' in a 'case' pattern}}
  ()
case S(x: 0, y: 0, comp: 0):
  ()
case S(w: 0): // expected-error{{property 'w' not found in type 'S'}}
  ()
case S(nonProperty: 0): // expected-error{{member 'nonProperty' of type 'S' is not a property}}
  ()
case S(0): // expected-error{{subpattern of a struct or class pattern must have a keyword name}}
  ()
case S(x: 0, 0): // expected-error{{subpattern of a struct or class pattern must have a keyword name}}
  ()

case T(): // expected-error{{type 'T' of pattern does not match deduced type 'S'}}
  ()
}

struct SG<A> { var x: A }

var sgs: SG<S>

switch sgs {
case SG(x: S()):
  ()
case SG<S>(x: S()):
  ()
case SG<T>(x: T()): // expected-error{{type 'SG<T>' of pattern does not match deduced type 'SG<S>'}}
  ()
}

func sg_generic<B : Equatable>(sgb: SG<B>, b: B) {
  switch sgb {
  case SG(x: b):
    ()
  }
}

typealias NonNominal = (foo: Int, bar: UnicodeScalar)
var nn = NonNominal.self

switch nn {
case NonNominal(): // expected-error{{non-nominal type 'NonNominal' cannot be used with property pattern syntax}}
  ()
}

// Tuple patterns.

var t = (1, 2, 3)

prefix operator +++ {}
infix operator +++ {}
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
case +++(_, var d, 3): // expected-error{{invalid pattern}}
  ()
case (_, var e, 3) +++ (1, 2, 3): // expected-error{{invalid pattern}}
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
let op1 : Int? = nil
let op2 : Int?? = nil

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
// expected-error @+4 {{refutable pattern match can fail; add an else {} to handle this condition}}
// expected-error @+3 {{label is not allowed on single element tuple pattern}}
// expected-note @+2 {{remove the parentheses to make this a type annotation}} {{5-6=}} {{26-27=}}
// expected-note @+1 {{remove the label to make this a tuple pattern}} {{6-21=}}
let (responseObject: Int?) = op1


