// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -target %target-swift-5.1-abi-triple -verify %s
protocol P {}
extension Int: P {}

func foo() -> some P { return 0 }
func bar<T: P>(_ x: T) -> some P { return x }

struct Bas<T: P> { init(_: T) {} }

func abstraction_level<T>(x: T) -> (T) -> () {
    return { _ in () }
}

func test() {
    abstraction_level(x: Bas(bar(foo())))(Bas(bar(foo())))
}
