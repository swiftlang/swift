// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

protocol P { }
protocol Q { }
typealias PQ = P & Q

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

func test13(body: (_ value: Int) -> Void, i: Int) {
  body(i)
}

func test14() {
  _ = Array<Array<Array<Int>>>().count
}

func testRepeatEach<each T>(_ t: repeat each T) -> (repeat each T) {
  fatalError()
}

// FIXME: this error isn't correct to emit. the parsing might be ignoring the ~
struct FileDescriptor: ~Copyable { // expected-error {{struct 'FileDescriptor' required to be 'Copyable' but is marked with '~Copyable'}}
  var fd = 1
}

// FIXME: warning for 'class'
protocol ClassOnly: class {}
