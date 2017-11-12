// RUN: %target-swift-frontend -primary-file %s -emit-ir

protocol P: AnyObject {}
protocol Foo {
    // The compiler crash goes away if you remove the P constraint on this associated type
    associatedtype ObjectType: P
}

protocol UpcastHelper {
    associatedtype Sub: Foo
    associatedtype Super: Foo

    // ObjectIdentifier(object) == ObjectIdentifier(Self.cast(object))
    static func cast(_ object: Sub.ObjectType) -> Super.ObjectType
}


struct AnyFoo<Object: P>: Foo {

    typealias ObjectType = Object

    class Base {}

    final class Derived<Helper: UpcastHelper>: Base where Helper.Super == AnyFoo<Object> {

        init(_ foo: Helper.Sub) {
            self.foo = foo
        }

        let foo: Helper.Sub
    }

    init<Helper: UpcastHelper>
        (foo: Helper.Sub, helper: Helper.Type)
        where Helper.Super == AnyFoo<Object> {

            // This is the expression that causes the crash
            _ = Derived<Helper>(foo)
    }
}
