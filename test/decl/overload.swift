// RUN: %target-parse-verify-swift

var var_redecl1: Int // expected-note {{previously declared here}}
var_redecl1 = 0
var var_redecl1: UInt // expected-error {{invalid redeclaration of 'var_redecl1'}}

var var_redecl2: Int // expected-note {{previously declared here}}
var_redecl2 = 0
var var_redecl2: Int // expected-error {{invalid redeclaration of 'var_redecl2'}}

var var_redecl3: (Int) -> () { get {} } // expected-note {{previously declared here}}
var var_redecl3: () -> () { get {} } // expected-error {{invalid redeclaration of 'var_redecl3'}}

var var_redecl4: Int // expected-note 2{{previously declared here}}
var var_redecl4: Int // expected-error {{invalid redeclaration of 'var_redecl4'}} 
var var_redecl4: Int // expected-error {{invalid redeclaration of 'var_redecl4'}}


let let_redecl1: Int = 0 // expected-note {{previously declared here}}
let let_redecl1: UInt = 0 // expected-error {{invalid redeclaration}}

let let_redecl2: Int = 0 // expected-note {{previously declared here}}
let let_redecl2: Int = 0 // expected-error {{invalid redeclaration}}


class class_redecl1 {} // expected-note {{previously declared here}}
class class_redecl1 {} // expected-error {{invalid redeclaration}}

class class_redecl2<T> {} // expected-note {{previously declared here}}
class class_redecl2 {} // expected-error {{invalid redeclaration}}

class class_redecl3 {} // expected-note {{previously declared here}}
class class_redecl3<T> {} // expected-error {{invalid redeclaration}}


struct struct_redecl1 {} // expected-note {{previously declared here}}
struct struct_redecl1 {} // expected-error {{invalid redeclaration}}

struct struct_redecl2<T> {} // expected-note {{previously declared here}}
struct struct_redecl2 {} // expected-error {{invalid redeclaration}}

struct struct_redecl3 {} // expected-note {{previously declared here}}
struct struct_redecl3<T> {} // expected-error {{invalid redeclaration}}


enum enum_redecl1 {} // expected-note {{previously declared here}}
enum enum_redecl1 {} // expected-error {{invalid redeclaration}}

enum enum_redecl2<T> {} // expected-note {{previously declared here}}
enum enum_redecl2 {} // expected-error {{invalid redeclaration}}

enum enum_redecl3 {} // expected-note {{previously declared here}}
enum enum_redecl3<T> {} // expected-error {{invalid redeclaration}}


protocol protocol_redecl1 {} // expected-note {{previously declared here}}
protocol protocol_redecl1 {} // expected-error {{invalid redeclaration}}


typealias typealias_redecl1 = Int // expected-note {{previously declared here}}
typealias typealias_redecl1 = Int // expected-error {{invalid redeclaration}}

typealias typealias_redecl2 = Int // expected-note {{previously declared here}}
typealias typealias_redecl2 = UInt // expected-error {{invalid redeclaration}}


var mixed_redecl1: Int // expected-note {{previously declared here}}
class mixed_redecl1 {} // expected-error {{invalid redeclaration}}

class mixed_redecl2 {} // expected-note {{previously declared here}}
struct mixed_redecl2 {} // expected-error {{invalid redeclaration}}

class mixed_redecl3 {} // expected-note {{previously declared here}}
enum mixed_redecl3 {} // expected-error {{invalid redeclaration}}

class mixed_redecl4 {} // expected-note {{previously declared here}}
protocol mixed_redecl4 {} // expected-error {{invalid redeclaration}}

class mixed_redecl5 {} // expected-note {{previously declared here}}
typealias mixed_redecl5 = Int // expected-error {{invalid redeclaration}}

func mixed_redecl6() {} // expected-note {{'mixed_redecl6()' previously declared here}}
var mixed_redecl6: Int // expected-error {{invalid redeclaration of 'mixed_redecl6'}}

var mixed_redecl7: Int // expected-note {{'mixed_redecl7' previously declared here}}
func mixed_redecl7() {} // expected-error {{invalid redeclaration of 'mixed_redecl7()'}}

func mixed_redecl8() {} // expected-note {{previously declared here}}
class mixed_redecl8 {} // expected-error {{invalid redeclaration}}

class mixed_redecl9 {} // expected-note {{previously declared here}}
func mixed_redecl9() {} // expected-error {{invalid redeclaration}}

func mixed_redecl10() {} // expected-note {{previously declared here}}
typealias mixed_redecl10 = Int // expected-error {{invalid redeclaration}}

typealias mixed_redecl11 = Int // expected-note {{previously declared here}}
func mixed_redecl11() {} // expected-error {{invalid redeclaration}}

var mixed_redecl12: Int // expected-note {{previously declared here}}
let mixed_redecl12: Int = 0 // expected-error {{invalid redeclaration}}

let mixed_redecl13: Int = 0 // expected-note {{previously declared here}}
var mixed_redecl13: Int // expected-error {{invalid redeclaration}}

var mixed_redecl14 : Int
func mixed_redecl14(i: Int) {} // okay

func mixed_redecl15(i: Int) {}
var mixed_redecl15 : Int  // okay

class OverloadStaticFromBase {
  class func create() {}
}
class OverloadStaticFromBase_Derived : OverloadStaticFromBase {
  class func create(x: Int) {}
}


// Overloading of functions based on argument names only.
func ovl_argname1(x x: Int, y: Int) { }
func ovl_argname1(y y: Int, x: Int) { }
func ovl_argname1(a a: Int, b: Int) { }

// Overloading with generics
protocol P1 { }
protocol P2 { }
func ovl_generic1<T: protocol<P1, P2>>(t t: T) { } // expected-note{{previous}}
func ovl_generic1<U: protocol<P1, P2>>(t t: U) { } // expected-error{{invalid redeclaration of 'ovl_generic1(t:)'}}

func ovl_generic2<T : P1>(_: T) {} // expected-note{{previously declared here}}
func ovl_generic2<T : P1>(_: T) {} // expected-error{{invalid redeclaration of 'ovl_generic2'}}

func ovl_generic3<T : P1>(x: T) {} // OK
func ovl_generic3<T : P2>(x: T) {} // OK

// Redeclarations within nominal types
struct X { }
struct Y { }
struct Z { 
  var a : X, // expected-note{{previously declared here}}
  a : Y // expected-error{{invalid redeclaration of 'a'}}
}

struct X1 {
  func f(a a : Int) {}  // expected-note{{previously declared here}}
  func f(a a : Int) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
}
struct X2 {
  func f(a a : Int) {} // expected-note{{previously declared here}}
  typealias IntAlias = Int
  func f(a a : IntAlias) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
}
struct X3 { 
  func f(a a : Int) {} // expected-note{{previously declared here}}
  func f(a a : IntAlias) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
  typealias IntAlias = Int
}

// Subscripting
struct Subscript1 {
  subscript (a: Int) -> Int {
    get { return a }
  }

  subscript (a: Float) -> Int {
    get { return Int(a) }
  }

  subscript (a: Int) -> Float {
    get { return Float(a) }
  }
}

struct Subscript2 {
  subscript (a: Int) -> Int { // expected-note{{previously declared here}}
    get { return a }
  }

  subscript (a: Int) -> Int { // expected-error{{invalid redeclaration of 'subscript'}}
    get { return a }
  }
}

// Initializers
class Initializers {
  init(x: Int) { } // expected-note{{previously declared here}}
  convenience init(x: Int) { } // expected-error{{invalid redeclaration of 'init(x:)'}}
}

// Default arguments
// <rdar://problem/13338746>
func sub(x x:Int64, y:Int64) -> Int64 { return x - y } // expected-note 2{{'sub(x:y:)' previously declared here}}
func sub(x x:Int64, y:Int64 = 1) -> Int64 { return x - y } // expected-error{{invalid redeclaration of 'sub(x:y:)'}}
func sub(x x:Int64 = 0, y:Int64 = 1) -> Int64 { return x - y } // expected-error{{invalid redeclaration of 'sub(x:y:)'}}

// <rdar://problem/13783231>
struct NoneType {
}

func != <T>(lhs : T, rhs : NoneType) -> Bool { // expected-note{{'!=' previously declared here}}
  return true
}

func != <T>(lhs : T, rhs : NoneType) -> Bool { // expected-error{{invalid redeclaration of '!=}}
  return true
}

// <rdar://problem/15082356>
func &&(lhs: BooleanType, @autoclosure rhs: ()->BooleanType) -> Bool { // expected-note{{previously declared}}
  return lhs.boolValue && rhs().boolValue
}

func &&(lhs: BooleanType, @autoclosure rhs: ()->BooleanType) -> Bool { // expected-error{{invalid redeclaration of '&&'}}
  return lhs.boolValue || rhs().boolValue
}

// throws
func throwsFunc(code code: Int) { } // expected-note{{previously declared}}
@noreturn func throwsFunc(code code: Int) throws { } // expected-error{{invalid redeclaration of 'throwsFunc(code:)'}}

// throws function parameter -- OK
func throwsFuncParam(fn: () throws -> ()) { }
func throwsFuncParam(fn: () -> ()) { }

// @noreturn
func noreturn(code code: Int) { } // expected-note{{previously declared}}
@noreturn func noreturn(code code: Int) { } // expected-error{{invalid redeclaration of 'noreturn(code:)'}}

// <rdar://problem/19816831>
func noreturn_1(x x: @noreturn (Int) -> Int) { } // expected-note{{previously declared}}
func noreturn_1(x x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'noreturn_1(x:)'}}

// @noescape
func noescape(@noescape x x: (Int) -> Int) { } // expected-note{{previously declared}}
func noescape(x x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'noescape(x:)'}}

// @autoclosure
func autoclosure(f f: () -> Int) { }
func autoclosure(@autoclosure f f: () -> Int) { }

// inout
func inout2(x x: Int) { }
func inout2(inout x x: Int) { }

// optionals
func optional(x x: Int?) { } // expected-note{{previously declared}}
func optional(x x: Int!) { } // expected-error{{invalid redeclaration of 'optional(x:)'}}

func optional_2(x x: (Int?) -> Int) { } // expected-note{{previously declared}}
func optional_2(x x: (Int!) -> Int) { } // expected-error{{invalid redeclaration of 'optional_2(x:)'}}

func optional_3() -> Int? { } // expected-note{{previously declared}}
func optional_3() -> Int! { } // expected-error{{invalid redeclaration of 'optional_3()'}}

// mutating / nonmutating
protocol ProtocolWithMutating {
  mutating func test1() // expected-note {{previously declared}}
  func test1() // expected-error{{invalid redeclaration of 'test1()'}}

  mutating func test2(a: Int?) // expected-note {{previously declared}}
  func test2(a: Int!) // expected-error{{invalid redeclaration of 'test2'}}

  @noreturn
  mutating func test3() // expected-note {{previously declared}}
  func test3() // expected-error {{invalid redeclaration of 'test3()'}}

  mutating static func classTest1() // expected-error {{static functions may not be declared mutating}} expected-note {{previously declared}}
  static func classTest1() // expected-error{{invalid redeclaration of 'classTest1()'}}
}

struct StructWithMutating {
  mutating func test1() { } // expected-note {{previously declared}}
  func test1() { } // expected-error{{invalid redeclaration of 'test1()'}}

  mutating func test2(a: Int?) { } // expected-note {{previously declared}}
  func test2(a: Int!) { } // expected-error{{invalid redeclaration of 'test2'}}

  @noreturn
  mutating func test3() { } // expected-note {{previously declared}}
  func test3() { } // expected-error {{invalid redeclaration of 'test3()'}}
}

enum EnumWithMutating {
  mutating func test1() { } // expected-note {{previously declared}}
  func test1() { } // expected-error{{invalid redeclaration of 'test1()'}}
}

