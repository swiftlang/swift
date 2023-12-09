
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen > %t/astgen.ast
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only > %t/cpp-parser.ast
// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test

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
  let
    xx = fn(42)

  let arlit = [0]
  let tuple = (0, 1)
  let diclit = [0: 1, 2: 3]

  return fn(x)
}

func testEmptyDictionary() -> [Int: Int] {
  return [:]
}

func test2(e b: Bool) {
  if b
  {
    print(
      "TRUE"
    )
  }
  else
  {
    print("FALSE")
  }

  let x =
    true
}

func test3(y: Int = 0, oi: Int? = nil) -> Int {
  let x =
    y
  return x
}

func test4(_ b: [Bool]) -> Int {
  if b.isEmpty { 0 } else {
    1
  }
}

func test5(_ b: Swift.Bool) -> Int {
  return if b { 0 } else { 1 }
}

func test6(_ b1: Bool, b2: Bool) -> Int {
  let x = if b1 { 0 } else if b2 { 1 } else { 2 }
  return x
}

func test7(_ b: inout Bool) {
  // b = true
}

func test8(_ i: _const Int) {
}

func test9(_ value: Any) { }

func test10<T>(t: T) where T: Proto1 {}

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

  enum Ty {
    case `self`
    case `Self`
  }

  func test() {
    _ = method(arg:_:)
    _ = self.method(arg:_:).self
    _ = Ty.`Self` ==  Ty.`self`
  }

// FIXME: Compute 'static' location
//  static var shared = TestStruct()
//  func testUnresolvedMember1() -> Self {
//    return .shared
//  }
//
// FIXME: Compute 'static' location
//  static func instance(arg: Int) -> TestStruct { return TestStruct() }
//  func testUnresolvedMember2() -> Self {
//    return .instance(arg:)(12)
//  }
}

func testSequence(arg1: Int, arg2: () -> Int, arg3: Any) {
  _ = arg1 + arg2()
  _ = arg3 as? Int ?? 31 as Int
  _ = false ? arg2() : Int(1)
  _ = [() -> Int]()
  _ = [@Sendable () -> Int]().count +  [any Collection]().count
  _ = arg3 is Double || !(arg3 is Int, 0).0
}

func asyncFunc(_ arg: String) async throws -> Int {
  return 1
}
func testUnaryExprs() async throws {
  let str = String()
  let foo = try await asyncFunc(_borrow str)
  let bar = copy foo
  let baz = consume foo
}

func testRepeatEach<each T>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}
