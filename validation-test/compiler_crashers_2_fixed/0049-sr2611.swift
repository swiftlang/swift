// RUN: %target-swift-frontend %s -emit-ir

protocol Foo {
    associatedtype A
    var value: A { get }
    init(_ v: A)
}
extension Foo {
    init<T>(pairing other: T)
        where
        T: Foo,
        Self.A == (T.A, T.A) // <-- Look at this, and then at the error below.
    {
        let otherValuePaired = (other.value, other.value)
        let v: A = otherValuePaired // <-- Error: Cannot convert value of
        self.init(v)                // type '(T.A, T.A)' to specified type 'Self.A'
    }
}
