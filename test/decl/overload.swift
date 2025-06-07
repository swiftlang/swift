// RUN: %target-typecheck-verify-swift

var var_redecl1: Int // expected-note {{previously declared here}}
var_redecl1 = 0
var var_redecl1: UInt // expected-error {{invalid redeclaration of 'var_redecl1'}}

var var_redecl2: Int // expected-note {{previously declared here}}
// expected-note@-1 {{found this candidate}}
var_redecl2 = 0 // expected-error {{ambiguous use of 'var_redecl2'}}
var var_redecl2: Int // expected-error {{invalid redeclaration of 'var_redecl2'}}
// expected-note@-1 {{found this candidate}}

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
// expected-error@-1 {{an enum with no cases cannot declare a raw type}}
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

var mixed_redecl16: Int // expected-note {{'mixed_redecl16' previously declared here}}
func mixed_redecl16() -> Int {} // expected-error {{invalid redeclaration of 'mixed_redecl16()'}}

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

  var b: X // expected-note{{previously declared here}}
}

extension Z {
  var b: Int { return 0 } // expected-error{{invalid redeclaration of 'b'}}
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
struct X4 {
  typealias i = Int
  // expected-note@-1 {{previously declared}}
  // expected-note@-2 {{previously declared}}

  static var i: String { return "" } // expected-error{{invalid redeclaration of 'i'}}
  static func i() {} // expected-error{{invalid redeclaration of 'i()'}}

  static var j: Int { return 0 } // expected-note{{previously declared here}}
  struct j {} // expected-error{{invalid redeclaration of 'j'}}

  var i: Int { return 0 }
  func i(x: String) {}
}

extension X4 {
  static var k: Int { return 0 } // expected-note{{previously declared here}}
  struct k {} // expected-error{{invalid redeclaration of 'k'}}
}

// Generic Placeholders
struct X5<t, u, v> {
  static var t: Int { return 0 }
  static func u() {}
  typealias v = String

  func foo<w>(_ w: w) {
    let w = w
    _ = w
  }
}

struct X6<T> {
  var foo: T // expected-note{{previously declared here}}
  func foo() -> T {} // expected-error{{invalid redeclaration of 'foo()'}}
  func foo(_ x: T) {}

  static var j: Int { return 0 } // expected-note{{previously declared here}}
  struct j {} // expected-error{{invalid redeclaration of 'j'}}
}

extension X6 {
  var k: Int { return 0 } // expected-note{{previously declared here}}
  func k()
  // expected-error@-1{{invalid redeclaration of 'k()'}}
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

  subscript (a: Int) -> Int { // expected-error{{invalid redeclaration of 'subscript(_:)'}}
    get { return a }
  }

  var `subscript`: Int { return 0 }
}

struct Subscript3 {
  typealias `subscript` = Int // expected-note{{previously declared here}}
  static func `subscript`(x: Int) -> String { return "" } // expected-error{{invalid redeclaration of 'subscript(x:)'}}

  func `subscript`(x: Int) -> String { return "" }
  subscript(x x: Int) -> String { return "" }
}

struct Subscript4 {
    subscript(f: @escaping (Int) -> Int) -> Int { // expected-note{{previously declared here}}
        get { return f(0) }
    }

    subscript(f: (Int) -> Int) -> Int { // expected-error{{invalid redeclaration of 'subscript(_:)'}}
        get { return f(0) }
    }
}

struct GenericSubscripts {
  subscript<T>(x: T) -> Int { return 0 } // expected-note{{previously declared here}}
}

extension GenericSubscripts {
  subscript<U>(x: U) -> Int { return 0 } // expected-error{{invalid redeclaration of 'subscript(_:)'}}
  subscript<T, U>(x: T) -> U { fatalError() }
  subscript<T>(x: T) -> T { fatalError() }
  subscript(x: Int) -> Int { return 0 }
}

struct GenericSubscripts2<T> {
  subscript(x: T) -> Int { return 0 } // expected-note{{previously declared here}}
}

extension GenericSubscripts2 {
  subscript(x: T) -> Int { return 0 } // expected-error{{invalid redeclaration of 'subscript(_:)'}}
  subscript<U>(x: U) -> Int { return 0 }
  subscript(x: T) -> T { fatalError() }
  subscript<U>(x: T) -> U { fatalError() }
  subscript<U, V>(x: U) -> V { fatalError() }
  subscript(x: Int) -> Int { return 0 }
}

struct GenericSubscripts3<T> {
  subscript<U>(x: T) -> U { fatalError() } // expected-note{{previously declared here}}
}

extension GenericSubscripts3 {
  subscript<U>(x: T) -> U { fatalError() } // expected-error{{invalid redeclaration of 'subscript(_:)'}}
  subscript<U, V>(x: U) -> V { fatalError() }
  subscript<U>(x: U) -> U { fatalError() }
  subscript(x: Int) -> Int { return 0 }
}

// Initializers
class Initializers {
  init(x: Int) { } // expected-note{{previously declared here}}
  convenience init(x: Int) { } // expected-error{{invalid redeclaration of 'init(x:)'}}

  static func `init`(x: Int) -> Initializers { fatalError() }
  func `init`(x: Int) -> Initializers { fatalError() }
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

// @_nonEphemeral
func nonEphemeral(x: UnsafeMutableRawPointer) {} // expected-note {{'nonEphemeral(x:)' previously declared here}}
func nonEphemeral(@_nonEphemeral x: UnsafeMutableRawPointer) {} // expected-error {{invalid redeclaration of 'nonEphemeral(x:)'}}

// inout
func inout2(x: Int) { }
func inout2(x: inout Int) { }

// optionals
func optional(x: Int?) { } // expected-note{{previously declared}}
func optional(x: Int!) { } 
// expected-error@-1{{invalid redeclaration of 'optional(x:)'}}
// expected-note@-2 {{implicitly unwrapped optional parameter is of same type as optional parameter}}

func optionalInOut(x: inout Int?) { } // expected-note{{previously declared}}
func optionalInOut(x: inout Int!) { } 
// expected-error@-1{{invalid redeclaration of 'optionalInOut(x:)'}}
// expected-note@-2 {{implicitly unwrapped optional parameter is of same type as optional parameter}}

class optionalOverloads {
  class func optionalInOut(x: inout Int?) { } // expected-note{{previously declared}}
  class func optionalInOut(x: inout Int!) { } 
  // expected-error@-1{{invalid redeclaration of 'optionalInOut(x:)'}}
  // expected-note@-2 {{implicitly unwrapped optional parameter is of same type as optional parameter}}

  func optionalInOut(x: inout Int?) { } // expected-note{{previously declared}}
  func optionalInOut(x: inout Int!) { } 
  // expected-error@-1{{invalid redeclaration of 'optionalInOut(x:)'}}
  // expected-note@-2 {{implicitly unwrapped optional parameter is of same type as optional parameter}}
}

func optional_3() -> Int? { } // expected-note{{previously declared}}
func optional_3() -> Int! { } // expected-error{{invalid redeclaration of 'optional_3()'}}

// mutating / nonmutating
protocol ProtocolWithMutating {
  mutating func test1() // expected-note {{previously declared}}
  func test1() // expected-error{{invalid redeclaration of 'test1()'}}

  mutating func test2(_ a: Int?) // expected-note {{previously declared}}
  func test2(_ a: Int!) // expected-error{{invalid redeclaration of 'test2'}}

  mutating static func classTest1() // expected-error {{static functions must not be declared mutating}} {{3-12=}} expected-note {{previously declared}}
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

protocol ProtocolWithAssociatedTypes {
  associatedtype t
  // expected-note@-1 {{previously declared}}
  // expected-note@-2 {{previously declared}}

  static var t: Int { get } // expected-error{{invalid redeclaration of 't'}}
  static func t() // expected-error{{invalid redeclaration of 't()'}}

  associatedtype u
  associatedtype v

  // Instance requirements are fine.
  var t: Int { get }
  func u()
  mutating func v()

  associatedtype W
  func foo<W>(_ x: W)
}

// <rdar://problem/21783216> Ban members named Type and Protocol without backticks
// https://twitter.com/jadengeller/status/619989059046240256
protocol r21783216a {
  // expected-error @+2 {{type member must not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{18-22=`Type`}}
  associatedtype Type
  
  // expected-error @+2 {{type member must not be named 'Protocol', since it would conflict with the 'foo.Protocol' expression}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{18-26=`Protocol`}}
  associatedtype Protocol
}

protocol r21783216b {
  associatedtype `Type`  // ok
  associatedtype `Protocol` // ok
}

// https://github.com/apple/swift/issues/49797

struct S_49797<T> {
  var x: T { get {} } // expected-note {{previously declared}}
  var y: Int // expected-note {{previously declared}}
  var z: Int // expected-note {{previously declared}}
}

extension S_49797 {
  var x: Int { get {} } // expected-warning{{redeclaration of 'x' is deprecated and will be an error in Swift 5}}
  var y: T { get {} } // expected-warning{{redeclaration of 'y' is deprecated and will be an error in Swift 5}}
  var z: Int { get {} } // expected-error{{invalid redeclaration of 'z'}}
}

// A constrained extension is okay.
extension S_49797 where T : P1 {
  var x: Int { get {} }
  var y: T { get {} }
  var z: Int { get {} }
}

// https://github.com/apple/swift/issues/49798

protocol P3 {
  var i: Int { get }
  subscript(i: Int) -> String { get }
}

extension P3 {
  var i: Int { return 0 }
  subscript(i: Int) -> String { return "" }
}

struct S_49798<T> : P3 {}

extension S_49798 where T : P3 {
  var i: Int { get {} }
  subscript(i: Int) -> String { get {} }
}

// https://github.com/apple/swift/issues/52486

struct S_52486 {
  let name: String
}

enum E_52486 {
  case foo(S_52486) // expected-note {{'foo' previously declared here}}
    
  static func foo(_ name: String) -> E_52486 { // Okay
    return .foo(S_52486(name: name))
  }

  func foo(_ name: Bool) -> E_52486 { // Okay
    return .foo(S_52486(name: "Test"))
  }

  static func foo(_ value: S_52486) -> E_52486 { // expected-error {{invalid redeclaration of 'foo'}}
    return .foo(value)
  }
}

enum E1_52486 {
  static func foo(_ name: String) -> E1_52486 { // Okay
    return .foo(S_52486(name: name))
  }

  static func foo(_ value: S_52486) -> E1_52486 { // expected-note {{'foo' previously declared here}}
    return .foo(value)
  }

  case foo(S_52486) // expected-error {{invalid redeclaration of 'foo'}}
}

enum E2_52486 {
  case fn(() -> Void) // expected-note {{'fn' previously declared here}}

  static func fn(_ x: @escaping () -> Void) -> E2_52486 {} // expected-error {{invalid redeclaration of 'fn'}}

  static func fn(_ x: @escaping () -> Int) -> E2_52486 {} // Okay

  static func fn(_ x: @escaping () -> Bool) -> E2_52486 {} // Okay
}

enum E3_52486 {
  protocol A {} // expected-note {{'A' previously declared here}}
  case A // expected-error {{invalid redeclaration of 'A'}}
}

enum E4_52486 {
  class B {} // expected-note {{'B' previously declared here}}
  case B // expected-error {{invalid redeclaration of 'B'}}
}

enum E5_52486 {
  struct C {} // expected-note {{'C' previously declared here}}
  case C // expected-error {{invalid redeclaration of 'C'}}
}

enum E6_52486 {
  case D // expected-note {{'D' previously declared here}}
  protocol D {} // expected-error {{invalid redeclaration of 'D'}}
}

enum E7_52486 {
  case E // expected-note {{'E' previously declared here}}
  class E {} // expected-error {{invalid redeclaration of 'E'}} 
}

enum E8_52486 {
  case F // expected-note {{'F' previously declared here}}
  struct F {} // expected-error {{invalid redeclaration of 'F'}} 
}

enum E9_52486 {
  case A // expected-note {{'A' previously declared here}} expected-note {{found this candidate}}
  static let A: E9_52486 = .A // expected-error {{invalid redeclaration of 'A'}}
  // expected-error@-1 {{ambiguous use of 'A'}} expected-note@-1 {{found this candidate}}
}

enum E10_52486 {
  static let A: E10_52486 = .A // expected-note {{'A' previously declared here}}
  // expected-error@-1 {{ambiguous use of 'A'}} expected-note@-1 {{found this candidate}}
  case A // expected-error {{invalid redeclaration of 'A'}} expected-note {{found this candidate}}
}

enum E11_52486 {
  case A // expected-note {{'A' previously declared here}} expected-note {{found this candidate}}
  static var A: E11_52486 = .A // expected-error {{invalid redeclaration of 'A'}}
  // expected-error@-1 {{ambiguous use of 'A'}} expected-note@-1 {{found this candidate}}
}

enum E12_52486 {
  static var A: E12_52486 = .A // expected-note {{'A' previously declared here}}
  // expected-error@-1 {{ambiguous use of 'A'}} expected-note@-1 {{found this candidate}}
  case A // expected-error {{invalid redeclaration of 'A'}} expected-note {{found this candidate}}
}

enum E13_52486 {
  case X // expected-note {{'X' previously declared here}}
  struct X<T> {} // expected-error {{invalid redeclaration of 'X'}}
}

enum E14_52486 {
  struct X<T> {} // expected-note {{'X' previously declared here}}
  case X // expected-error {{invalid redeclaration of 'X'}}
}

enum E15_52486 {
  case Y // expected-note {{'Y' previously declared here}}
  typealias Y = Int // expected-error {{invalid redeclaration of 'Y'}}
}

enum E16_52486 {
  typealias Z = Int // expected-note {{'Z' previously declared here}}
  case Z // expected-error {{invalid redeclaration of 'Z'}}
}
