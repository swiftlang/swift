// RUN: %target-typecheck-verify-swift -enable-experimental-feature ASTGenTypes

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts


func test7(_ b: inout Bool) {
  b = true
}

struct X { struct `Protocol` { } }

func test10(_: X.`Protocol`) { }

func test11(_: Int...) { }
func test11a() {
  test11(1, 2, 3, 4, 5)
}

typealias VAFunc = (Int, Int...) -> Int
func testVAFunc(a: Int, f: VAFunc) {
  _ = f(a, a, a, a, a)
}

func test12(_ producer: @escaping @autoclosure () -> Int) {
  _ = producer()
}
func test12a(i: Int) {
  test12(i)
}
