// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse -target %target-swift-5.1-abi-triple -enable-experimental-move-only -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature OldOwnershipOperatorSpellings \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse -target %target-swift-5.1-abi-triple -enable-experimental-move-only \
// RUN:   -enable-experimental-feature OldOwnershipOperatorSpellings \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-target %target-swift-5.1-abi-triple -enable-experimental-feature OldOwnershipOperatorSpellings -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_OldOwnershipOperatorSpellings

// rdar://116686158
// UNSUPPORTED: asan

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

func test1(x: Int, fn: (Int) -> Int) async throws -> Int {
  let
  xx = fn(42)

  let y =
    true

  let arlit = [0]
  let tuple = (0, -21.4)
  let diclit = [0: 1, 2: -3]

  return fn(x)
}

func test2(_ b: Swift.Bool) -> Int {
  return if b { 0 } else { 1 }
}

func test3(_ b1: Bool, b2: Bool) -> Int {
  let x = if b1 { 0 } else if b2 { 1 } else { 2 }
  return x
}

func test4(_ b: [Bool]) -> Int {
  if b.isEmpty { 0 } else {
    1
  }
}

func testEmptyDictionary() -> [Int: Int] {
  return [:]
}

enum Ty {
  case `self`
  case `Self`
}

struct TestStruct {
  func method(arg: Int, _ c: Int) {}
  
  func test() {
    _ = method(arg:_:)
    _ = self.method(arg:_:).self
    _ = Ty.`Self` ==  Ty.`self`
  }

  var optSelf: Self? { self }
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
func throwsFunc() throws -> Int { 1 }
func testOptionalTry() {
  let _ = try! 1 + throwsFunc()
  let _ = try? throwsFunc() + throwsFunc()
}

func testRepeatEach<each T>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}

func acceptClosures(x: () -> Void) {}
func acceptClosures(x: () -> Void, y: () -> Int) {}
func acceptClosures(x: () -> Void, y: () -> Int, _ z: () -> Void) {}
func acceptClosures(x: (Int, String) -> Void) {}
func acceptClosures(x: (Int, String, inout String) -> Void) {}
func testTrailingClsure() {
  acceptClosures {}
  acceptClosures() {}
  acceptClosures(x: {}) { 42 }
  acceptClosures(x: {}) { 12 } _: {}
  acceptClosures {} y: { 42 }
  acceptClosures(x: {}, y: { 12 }) {}

  acceptClosures { (x, y: String) -> Void in  }
  acceptClosures { x, y in  }
  acceptClosures { @Sendable x, y in  }

  acceptClosures { $1.count == $0.bitWidth }
  acceptClosures { $1.count }
  acceptClosures {
    _ = $1
    acceptClosures { $2 = $1 }
  }
}
class TestClosure {
  func testCaptureList() {
    var val = self
    var str = "t"
    acceptClosures { [self] in _ = self }
    acceptClosures { [self] in _ = $1 }
    acceptClosures { [foo = self] in }
    acceptClosures { [self = self] in }
    acceptClosures { [weak self] in }
    acceptClosures { [unowned self] in }
    acceptClosures { [unowned(safe) self] in }
    acceptClosures { [unowned(unsafe) self] in }
    acceptClosures { [unowned(unsafe) self = val] in }
    acceptClosures { [val = self] in }
    acceptClosures { [val = self, str, self = val] in }
  }
}

func testInOut() {
  func acceptInOut(arg: inout Int) { arg += 1 }
  var value = 42
  acceptInOut(arg: &value)
}

func testStringLiteral(arg: Int) {
  _ = "test"
  _ = "foo\(arg)bar"
  _ = "\(arg)"
  _ = """
    foo
    bar\
    baz
    """
  _ = """
    foo\(arg)
    \(arg)bar
    """
  _ = "\n\r\u{1234}"
  _ = """
    foo
    \(
      ("bar", """
         \tbaz\u{0d}
         """
      )
    )
    baz
    """
}

func testNumberLiteral() {
  _ = 12
  _ = 1_2
  _ = 0xab
  _ = 0xab_p2
  _ = 12.42
  _ = 0b0000_1100_1000
  _ = 1_
  _ = 1_000
  _ = 0b1111_0000_
  _ = 0b1111_0000
  _  = 0o127_777_
  _ = 0o127_777
  _ = 0x12FF_FFFF
  _ = 0x12FF_FFFF_
  _ = 1.0e42
  _ = 0x1.0p0
  _ = 0x1.fffffep+2
  _ = 1_000.200_001e1_000
  _ =  0x1_0000.0FFF_ABCDp10_001
}

class BaseCls {
  init(base: Int) {}
}
class DerivedCls: BaseCls {
  init(testSuperRef arg: Int) { super.init(base: arg) }
}

struct HasSubscript {
  subscript(label label: Int, args: Int) -> Int { return 1 }
}
func testSubscript(intArry: [Int], val: HasSubscript) {
  _ = intArry[12]
  _ = val[label: 42, 14]
}

struct Generic<T: Comparable> {}
func testSpecializeExpr() {
  _ = Generic<Int>.self
  _ = Generic<Int>()
}

func testOptionalChain(value: TestStruct) {
  let _: TestStruct? = value.optSelf?.optSelf!
  let _: TestStruct = value.optSelf!
  let _: TestStruct = value.optSelf.self!

  var value: Int? = 1
  value? += 1
}

func testSwitchExpr(value: Int) {
  let _ = switch value {
    case 0: "foo"
    case ...100: "bar"
    default: "baz"
  }
}
