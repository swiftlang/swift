// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend-dump-parse -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature NamedOpaqueTypes \
// RUN:   -enable-experimental-feature InlineArrayTypeSugar \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: %target-swift-frontend-dump-parse \
// RUN:   -enable-experimental-feature NamedOpaqueTypes \
// RUN:   -enable-experimental-feature InlineArrayTypeSugar \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen \
// RUN:   -enable-experimental-feature NamedOpaqueTypes \
// RUN:   -enable-experimental-feature InlineArrayTypeSugar

// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_NamedOpaqueTypes
// REQUIRES: swift_feature_InlineArrayTypeSugar

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

struct FileDescriptor: ~Copyable {
  var fd = 1
}

// FIXME: warning for 'class'
protocol ClassOnly: class {}

actor SomeActor { }
@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}
typealias SomeGlobalActorIsolated = @SomeGlobalActor () -> Void
typealias TestSpecifiers<Value, Result, E> = (inout sending Value) throws(E) -> sending Result where Value: ~Copyable, Result: ~Copyable, E: Error
typealias TestSpecifierAndAttr<T> = (__owned @Sendable @escaping () async -> T) -> T

let globalOptionalInt: _? = 42
let optionalIntArray: Array<_> = [42]

@available(SwiftStdlib 9999, *)
func testInlineArray() {
  let _: [3 of Int] = [1, 2, 3]
  let _: [_ of _] = [1, 2]
  let _ = [3 of Int](repeating: 0)
  let _ = [3 of _](repeating: 0)
}

func testNamedOpaqueReturnTy() -> <T> T { return () }
