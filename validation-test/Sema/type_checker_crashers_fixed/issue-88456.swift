// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/88456

func divide(_ a: Int, byDividend b: Int) -> Int { return a / b }

var f: (_: Int, _ byDividend: Int) -> Int = divide // expected-note {{'f' previously declared here}}

@preconcurrency @MainActor func f() { } // expected-error {{invalid redeclaration of 'f()'}}

@preconcurrency typealias OtherHandler = @Sendable () -> Void
@preconcurrency typealias Handler = (@Sendable () -> OtherHandler?)?
@preconcurrency func f(arg: Int, withFn: Handler?) {} // expected-note {{'f(arg:withFn:)' declared here}}

func test() {
  var _: (@Sendable () -> Void) = { f() } // expected-error {{missing arguments for parameters 'arg', 'withFn' in call}}
}
