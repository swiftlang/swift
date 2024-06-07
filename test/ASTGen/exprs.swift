
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

func test1(x: Int, fn: (Int) -> Int) async throws -> Int {
  let
  xx = fn(42)

  let y =
    true

  let arlit = [0]
  let tuple = (0, 1)
  let diclit = [0: 1, 2: 3]

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

func acceptClosures(x: () -> Void) {}
func acceptClosures(x: () -> Void, y: () -> Int) {}
func acceptClosures(x: () -> Void, y: () -> Int, _ z: () -> Void) {}
func testTrailingClsure() {
  acceptClosures {}
  acceptClosures() {}
  acceptClosures(x: {}) { 42 }
  acceptClosures(x: {}) { 12 } _: {}
  acceptClosures {} y: { 42 }
  acceptClosures(x: {}, y: { 12 }) {}
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
