// RUN: %target-swift-frontend -emit-sil -sil-verify-all %s
// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all %s

protocol Foo {
    var foo: String { get }
} 
 
func identity(_ a: consuming any Foo) -> String {
    return a.foo
}

// https://github.com/swiftlang/swift/issues/87489
protocol P: ~Copyable {}
struct S: P, ~Copyable {}

func process<T: P & ~Copyable>(p: consuming T) {}

func foo() {
    let p: any P & ~Copyable = S()
    process(p: p)
}
