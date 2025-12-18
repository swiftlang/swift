// RUN: %target-typecheck-verify-swift

public protocol P {
    associatedtype A
    associatedtype B: P

    var b: B { get }
    static func f() -> Any?
}

protocol Q: P where B == Never {} // expected-error@:10 {{circular reference}}
// expected-warning@+1 {{property 'b' must be as accessible as its enclosing type because it matches a requirement in protocol 'P'}}
extension Never: Q, P { // expected-note@:1 {{through reference here}}
   public typealias A = Never
   public static func f() -> Any? { nil }
}

extension Q {
// expected-note@+1 {{mark the property as 'public' to satisfy the requirement}}
   public var b: Never { fatalError() } // expected-note {{through reference here}}
}

