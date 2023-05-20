
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-feature ParserASTGen > %t/astgen.ast
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking > %t/cpp-parser.ast
// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature SwiftParser -enable-experimental-feature ParserASTGen)
// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking -enable-experimental-feature ASTGenTypes)

// REQUIRES: executable_test

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts

// NB: Ridiculous formatting to test that we do not include leading trivia in locations.

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

  return fn(x)
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

func test6(_ b: Bool) -> Int {
  let x = if b { 0 } else { 1 }
  return x
}

func test7(_ b: inout Bool) {
  // b = true
}

func test8(_ i: _const Int) {
}

func test9(_ value: Any) { }

func test10<T>(t: T) {}

typealias
Alias<T>
=
String

struct
Struct
<
  T1,
  T2
>
{
  /*static*/ func method(_ b: Bool) {}
}

class
Class<T> {
  func method(_ b: Bool) {}
}
