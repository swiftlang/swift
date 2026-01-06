
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-concurrency -enable-experimental-feature ParserASTGen \
// RUN:    -enable-experimental-feature CoroutineAccessors \
// RUN:    -enable-experimental-feature DefaultIsolationPerFile \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-concurrency \
// RUN:    -enable-experimental-feature CoroutineAccessors \
// RUN:    -enable-experimental-feature DefaultIsolationPerFile \
// RUN:    | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -Xfrontend -enable-experimental-concurrency -enable-experimental-feature CoroutineAccessors -enable-experimental-feature DefaultIsolationPerFile -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_DefaultIsolationPerFile

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


using @MainActor
// FIXME: cannot add `using nonisolated` here because it's a re-declaration

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
  b = true
}

func testInOutClosureParam() -> (inout (Int, String)) -> Void {
  return { (arg: inout (Int, String)) in
    arg.1 = "rewritten"
  }
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
  var t: Int {
    read { yield q }
    modify { yield &q }
  }
}

func rethrowingFn(fn: () throws -> Void) rethrows {}
func reasyncFn(fn: () async -> Void) reasync {}
func testRethrows() {
    rethrowingFn { _ = 1 }

    // FIXME: Assertion failed: (isAsync()), function getAsyncContext, file GenCall.cpp, line 215.
    // reasyncFn { _ = 1 }
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

  private(set) var testPrivateSet = 1
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

  subscript<I: Proto3, J: Proto3>(i: I, j: J) -> Int where I.A == J.B {
    1
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

func concreteValueTest1(_: ValueStruct<123>) {}
func concreteValueTest2(_: ValueStruct<-123>) {}

extension ValueStruct where N == 123 {}
extension ValueStruct where 123 == N {}
extension ValueStruct where N == -123 {}
extension ValueStruct where -123 == N {}

func testMagicIdentifier(file: String = #file, line: Int = #line) {
    let _: String = file
    let _: Int = line
}

class StaticTest {
  class var classVar: Int { 42 }
  static let staticVar = 42
}

func defaultArgInitTestFunc(fn: () -> () = {}) {}
struct DefaultArgInitTestStruct {
  func foo(fn: () -> () = {}) {}
}
enum DefaultArgInitTestEnum {
    case foo(x: () -> () = {})
}

@resultBuilder
struct MyBuilder {
static func buildBlock(_ items: Any...) -> [Any] { items }
}
func acceptBuilder(@MyBuilder fn: () -> [Any]) -> [Any] { fn() }
func testBuilder() {
  let _: [Any] = acceptBuilder {
    1
  }
}

struct ExpansionRequirementTest<each T> where repeat each T: Comparable {}
