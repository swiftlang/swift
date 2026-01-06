// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ThenStatements -enable-experimental-feature ParserASTGen \
// RUN:    | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ThenStatements \
// RUN:    | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature ThenStatements -enable-experimental-feature ParserASTGen)

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_ThenStatements

// rdar://116686158
// UNSUPPORTED: asan

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

func test1(e b: Bool) {
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
}

struct UnknownError: Error {}

func testSwitch(arg1: Int, arg2: Int) throws {
  LABEL: switch (arg1, arg2) {
  case (1, 2):
    print("")
    fallthrough
  case (1, _) where true, (4, 3):
    break LABEL
  case (var foo, let bar) where foo.isMultiple(of: 2):
    defer {
      print(foo)
    }
    foo += 1
    print(bar)
    break
  default:
    throw UnknownError()
  }
}

func canThrow() throws {}

func testDo() {
  do {
    try canThrow() 
  } catch is UnknownError {
    return 
  } catch where error is UnknownError {
    return 
  } catch let error, {
    _ = error
    return 
  } catch {
    return
  }

  LOOP: do {
    print("foo")
    if true {
      continue  LOOP
    }
  }
}

func testGuard(arg1: Int?, arg2: String) {
  guard let arg1, var arg2First = arg2.first, arg1 > 2 else {
    return
  }
  arg2First = "a"

  print(arg1, arg2First)
}

func testFor(arg1: [Int?]) {
  for _ in 0..<1 { }
  for case let .some(elem) in arg1 where elem < 42 { print(elem) }
  for var elem in [1,2,3] {
    elem += 1
    print(elem)
  }
  for (a, b) in [(1,2)] {}
}

func testRepeat() {
  repeat {
    print(1)
  } while true
}

func testRepeat(arg: [Int]) {
  var iter = arg.makeIterator()
  while let a = iter.next(), a > Int.random(in: 0..<10) {
    print(a)
  }
}

func testThen() {
  let x: Int = if .random() {
    then .zero
  } else {
    then 0
  }
}

func intOrString() -> Int? { 1 }
func intOrString() -> String? { "" }
func testIf() {
  if
    let i: Int = intOrString(),
    case let str? = intOrString() as String?
  {
    _ = (i, str)
  }
}

struct GenericTypeWithYields<T> {
  var storedProperty: T?

  var property: T {
    _read {
      yield storedProperty!
    }
    _modify {
      yield &storedProperty!
    }
  }

/* FXIME: yield(...) is parsed as an expression. ASTGen needs to onvert it to a statments.
  subscript<U>(u: U) -> (T,U) {
    _read {
      yield ((storedProperty!, u))
    }
    _modify {
      var temp = (storedProperty!, u)
      yield &temp
    }
  }
  */
}
