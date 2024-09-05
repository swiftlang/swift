
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen -enable-experimental-feature ValueGenerics > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ValueGenerics > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen -enable-experimental-feature ValueGenerics)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

import
Swift
import typealias Swift.Codable
import enum Swift.Optional
import struct Swift.Array
import class Swift.KeyPath
import protocol Swift.Sequence
import func Swift.max
import var Swift._playgroundPrintHook

func
test1
(
  y
    x: Int, fn: (Int) -> Int
)
async
throws
->
Int {
  return 0
}

func test2(y: Int = 0, oi: Int? = nil) -> Int {
  let x =
    y
  return x
}

func test3(_ b: inout Bool) {
  // b = true
}

func test4(_ i: _const Int) {
}

func test5(_ value: Any) { }

func test6<T>(t: T) where T: Proto1 {}

func test7() {
  var binding1 = 0, binding2 = ""
}

func test8(_: Int) {}

func test9() -> Int { 0 }

func testVars() {
  var a = 0
  var b: Int = 0
  var c, d: Int
  var e, f: Int, g, h, i: String
  let j: Int = 0, k: String = ""

  var l: Int { 0 }
  var m: Int { get { 0 } }
  var n: Int {
    get { return m }
    set {}
  }
  var o: Int = 0 {
    willSet {
      n = newValue
    }
  }
  var p: Int {
    get { return 0 }
    set(foo) {
      o = foo
    }
  }
  var q: Int = 0 {
    didSet(old) {
      p = old
    }
  }
  var r: Int {
    _read { yield q }
    _modify { yield &q }
  }
  var s: Int {
    get async throws { return 0 }
  }
}

struct TestVars {
  var a = 0
  var b: Int = 0
  var c, d: Int
  var e, f: Int, g, h, i: String
  let j: Int = 0, k: String = ""

  var l: Int { 0 }
  var m: Int { get { 0 } }
  var n: Int {
    get { return m }
    set {}
  }
  var o: Int = 0 {
    willSet {
      n = newValue
    }
  }
  var p: Int {
    get { return 0 }
    set(foo) {
      o = foo
    }
  }
  var q: Int = 0 {
    didSet(old) {
      p = old
    }
  }
  var r: Int {
    _read { yield q }
    _modify { yield &q }
  }
  var s: Int {
    get async throws { return 0 }
  }
}

extension TestVars {
  var inExt: Int { return 0 }
}

struct TestSubscripts {
  subscript(x: Int) -> Int {
    0
  }
  subscript(y x: Int) -> Int {
    get {
      return 0
    }
    set(x) {}
  }
}

protocol Proto1 {}
protocol Proto2 {}

protocol
Proto3
<
  A,
  B
>: Proto1 where Self: Proto2 {
  associatedtype
    A where B: Proto1
  associatedtype B: Equatable
    =
    Int

  func method(_ b: Bool)
}

typealias
Alias<T>
=
String where T: Proto1

enum
Enum<T>: Int, Proto1 where T: Proto1 {
  case
  a
    =
    1
  case b = 2,
       c

  func method(_ b: Bool) {}
}

enum WithPayload {
  case a(
    b: Int
  ), c(
    d: Bool = false
  )
}

struct
Struct
<
  T1:
  Proto1,
  T2:
  Proto2
>
:
Proto1, Proto2, @unchecked Sendable
where
  T1
  :
  Proto3,
  T1.A
  ==
  Int
{
  /*static*/ func method(_ b: Bool) {}
}

class
Class<T>: Proto1 where T: Proto3 {
  func method(_ b: Bool) {}

  deinit {
    if true {}
  }

  init?<U>(_ u: U) where U: Proto1 {
    if true {}
  }

  init!(i: Int) {}
}

actor
Actor<T>: Proto1 where T: Proto1 {
  func method(_ b: Bool) {}
}

extension
Class: Proto2 where T: Proto1 {
  func method2(_ b: Bool) {}
}

prefix
operator ⎭^-^⎭

infix
operator
  ~^-^~
  :
  AdditionPrecedence

postfix
operator ⎩^-^⎩

precedencegroup Precedence1 {
}

precedencegroup Precedence2 {
  lowerThan: BitwiseShiftPrecedence, MultiplicationPrecedence
  higherThan: Precedence1, AdditionPrecedence
  associativity: left
  assignment: true
}

struct TestStruct {
  func method(arg: Int, _ c: Int) {}

// FIXME: Compute 'static' location
//  static var shared = TestStruct()
//  func testUnresolvedMember1() -> Self {
//    return .shared
//  }
//
// FIXME: Compute 'static' location
//  static func instance(arg: Int) -> TestStruct { return TestStruct() }
}

struct ValueStruct<let N: Int> {}

func genericTest1<T>(_: T) {}
func genericTest2<each T>(_: repeat each T) {}
func genericTest4<let T: Int>(_: ValueStruct<T>) {}
