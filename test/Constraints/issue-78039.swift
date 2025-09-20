// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/78039
protocol A {
    associatedtype Node
}

extension A {
    func foo(arr: inout Node) { }

    func bar(_ f: (inout Node) -> Void = foo) { }
    // expected-error@-1 {{default argument value of type '(Self) -> (inout Self.Node) -> ()' cannot be converted to type '(inout Self.Node) -> Void'}}
    // expected-error@-2 {{generic parameter 'Self' could not be inferred}}
}
