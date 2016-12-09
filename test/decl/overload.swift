// RUN: %target-typecheck-verify-swift

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
class mixed_redecl1a : mixed_redecl1 {}

class mixed_redecl2 {} // expected-note {{previously declared here}}
struct mixed_redecl2 {} // expected-error {{invalid redeclaration}}

class mixed_redecl3 {} // expected-note {{previously declared here}}
// expected-note @-1 2{{found this candidate}}
enum mixed_redecl3 {} // expected-error {{invalid redeclaration}}
// expected-note @-1 2{{found this candidate}}
enum mixed_redecl3a : mixed_redecl3 {} // expected-error {{'mixed_redecl3' is ambiguous for type lookup in this context}}
// expected-error @-1{{'mixed_redecl3a' does not conform}}
class mixed_redecl3b : mixed_redecl3 {} // expected-error {{'mixed_redecl3' is ambiguous for type lookup in this context}}

class mixed_redecl4 {} // expected-note {{previously declared here}}
// expected-note@-1{{found this candidate}}
protocol mixed_redecl4 {} // expected-error {{invalid redeclaration}}
// expected-note@-1{{found this candidate}}
protocol mixed_redecl4a : mixed_redecl4 {} // expected-error {{'mixed_redecl4' is ambiguous for type lookup in this context}}

class mixed_redecl5 {} // expected-note {{previously declared here}}
typealias mixed_redecl5 = Int // expected-error {{invalid redeclaration}}
typealias mixed_redecl5a = mixed_redecl5

func mixed_redecl6() {} // expected-note {{'mixed_redecl6()' previously declared here}}
var mixed_redecl6: Int // expected-error {{invalid redeclaration of 'mixed_redecl6'}}

var mixed_redecl7: Int // expected-note {{'mixed_redecl7' previously declared here}}
func mixed_redecl7() {} // expected-error {{invalid redeclaration of 'mixed_redecl7()'}}

func mixed_redecl8() {} // expected-note {{previously declared here}}
class mixed_redecl8 {} // expected-error {{invalid redeclaration}}
class mixed_redecl8a : mixed_redecl8 {}

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
func mixed_redecl14(_ i: Int) {} // okay

func mixed_redecl15(_ i: Int) {}
var mixed_redecl15 : Int  // okay

class OverloadStaticFromBase {
  class func create() {}
}
class OverloadStaticFromBase_Derived : OverloadStaticFromBase {
  class func create(_ x: Int) {}
}


// Overloading of functions based on argument names only.
func ovl_argname1(x: Int, y: Int) { }
func ovl_argname1(y: Int, x: Int) { }
func ovl_argname1(a: Int, b: Int) { }

// Overloading with generics
protocol P1 { }
protocol P2 { }
func ovl_generic1<T: P1 & P2>(t: T) { } // expected-note{{previous}}
func ovl_generic1<U: P1 & P2>(t: U) { } // expected-error{{invalid redeclaration of 'ovl_generic1(t:)'}}

func ovl_generic2<T : P1>(_: T) {} // expected-note{{previously declared here}}
func ovl_generic2<T : P1>(_: T) {} // expected-error{{invalid redeclaration of 'ovl_generic2'}}

func ovl_generic3<T : P1>(_ x: T) {} // OK
func ovl_generic3<T : P2>(_ x: T) {} // OK

// Redeclarations within nominal types
struct X { }
struct Y { }
struct Z { 
  var a : X, // expected-note{{previously declared here}}
  a : Y // expected-error{{invalid redeclaration of 'a'}}
}

struct X1 {
  func f(a : Int) {}  // expected-note{{previously declared here}}
  func f(a : Int) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
}
struct X2 {
  func f(a : Int) {} // expected-note{{previously declared here}}
  typealias IntAlias = Int
  func f(a : IntAlias) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
}
struct X3 { 
  func f(a : Int) {} // expected-note{{previously declared here}}
  func f(a : IntAlias) {} // expected-error{{invalid redeclaration of 'f(a:)'}}
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
func sub(x:Int64, y:Int64) -> Int64 { return x - y } // expected-note 2{{'sub(x:y:)' previously declared here}}
func sub(x:Int64, y:Int64 = 1) -> Int64 { return x - y } // expected-error{{invalid redeclaration of 'sub(x:y:)'}}
func sub(x:Int64 = 0, y:Int64 = 1) -> Int64 { return x - y } // expected-error{{invalid redeclaration of 'sub(x:y:)'}}

// <rdar://problem/13783231>
struct NoneType {
}

func != <T>(lhs : T, rhs : NoneType) -> Bool { // expected-note{{'!=' previously declared here}}
  return true
}

func != <T>(lhs : T, rhs : NoneType) -> Bool { // expected-error{{invalid redeclaration of '!=}}
  return true
}


// throws
func throwsFunc(code: Int) { } // expected-note{{previously declared}}
func throwsFunc(code: Int) throws { } // expected-error{{invalid redeclaration of 'throwsFunc(code:)'}}

// throws function parameter -- OK
func throwsFuncParam(_ fn: () throws -> ()) { }
func throwsFuncParam(_ fn: () -> ()) { }

// @escaping
func escaping(x: @escaping (Int) -> Int) { } // expected-note{{previously declared}}
func escaping(x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping(x:)'}}
func escaping(_ x: @escaping (Int) -> Int) { } // expected-note{{previously declared}}
func escaping(_ x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping'}}
func escaping(a: Int, _ x: @escaping (Int) -> Int) { } // expected-note{{previously declared}}
func escaping(a: Int, _ x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping(a:_:)'}}

func escaping(_ a: (Int) -> Int, _ x: (Int) -> Int) { }
  // expected-note@-1{{previously declared}}
  // expected-note@-2{{previously declared}}
  // expected-note@-3{{previously declared}}
func escaping(_ a: (Int) -> Int, _ x: @escaping (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping'}}
func escaping(_ a: @escaping (Int) -> Int, _ x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping'}}
func escaping(_ a: @escaping (Int) -> Int, _ x: @escaping (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping'}}

struct Escaping {
  func escaping(_ x: @escaping (Int) -> Int) { } // expected-note{{previously declared}}
  func escaping(_ x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping'}}
  func escaping(a: Int, _ x: @escaping (Int) -> Int) { } // expected-note{{previously declared}}
  func escaping(a: Int, _ x: (Int) -> Int) { } // expected-error{{invalid redeclaration of 'escaping(a:_:)'}}
}

// @autoclosure
func autoclosure(f: () -> Int) { }
func autoclosure(f: @autoclosure () -> Int) { }

// inout
func inout2(x: Int) { }
func inout2(x: inout Int) { }

// optionals
func optional(x: Int?) { } // expected-note{{previously declared}}
func optional(x: Int!) { } // expected-error{{invalid redeclaration of 'optional(x:)'}}

func optional_2(x: (Int?) -> Int) { } // expected-note{{previously declared}}
func optional_2(x: (Int!) -> Int) { } // expected-error{{invalid redeclaration of 'optional_2(x:)'}}

func optional_3() -> Int? { } // expected-note{{previously declared}}
func optional_3() -> Int! { } // expected-error{{invalid redeclaration of 'optional_3()'}}

// mutating / nonmutating
protocol ProtocolWithMutating {
  mutating func test1() // expected-note {{previously declared}}
  func test1() // expected-error{{invalid redeclaration of 'test1()'}}

  mutating func test2(_ a: Int?) // expected-note {{previously declared}}
  func test2(_ a: Int!) // expected-error{{invalid redeclaration of 'test2'}}

  mutating static func classTest1() // expected-error {{static functions may not be declared mutating}} {{3-12=}} expected-note {{previously declared}}
  static func classTest1() // expected-error{{invalid redeclaration of 'classTest1()'}}
}

struct StructWithMutating {
  mutating func test1() { } // expected-note {{previously declared}}
  func test1() { } // expected-error{{invalid redeclaration of 'test1()'}}

  mutating func test2(_ a: Int?) { } // expected-note {{previously declared}}
  func test2(_ a: Int!) { } // expected-error{{invalid redeclaration of 'test2'}}
}

enum EnumWithMutating {
  mutating func test1() { } // expected-note {{previously declared}}
  func test1() { } // expected-error{{invalid redeclaration of 'test1()'}}
}

// <rdar://problem/21783216> Ban members named Type and Protocol without backticks
// https://twitter.com/jadengeller/status/619989059046240256
protocol r21783216a {
  // expected-error @+2 {{type member may not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{18-22=`Type`}}
  associatedtype Type
  
  // expected-error @+2 {{type member may not be named 'Protocol', since it would conflict with the 'foo.Protocol' expression}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{18-26=`Protocol`}}
  associatedtype Protocol
}

protocol r21783216b {
  associatedtype `Type`  // ok
  associatedtype `Protocol` // ok
}



