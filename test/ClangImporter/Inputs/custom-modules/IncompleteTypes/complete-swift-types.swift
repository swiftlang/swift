import Foundation

@objc public class Foo : NSObject {
    @objc public func sayHello() {
        print("Hello from Foo.sayHello!")
    }
}

@objc(Baz) public class Bar : NSObject {
    @objc public func sayHello() {
        print("Hello from Bar.sayHello!")
    }
}

class ConflictingTypeName {}

@objc(ConflictingTypeName) public class Qux : NSObject {
    @objc public func sayHello() {
        print("Hello from Qux.sayHello!")
    }
}

// Created to verify if using a special
// name as an @objc identifier causes issues
@objc(subscript) public class Corge : NSObject {
    @objc public func sayHello() {
        print("Hello from Corge.sayHello!")
    }
}

@objc protocol ShadowedProtocol {}

@objc public protocol ProtocolFoo {}
@objc(ProtocolBaz) public protocol Bam {}
protocol ProtocolConflictingTypeName {}
@objc(ProtocolConflictingTypeName) public protocol Quux  { }

@objc public class ProtocolConformer : NSObject, ProtocolFoo, Quux, ProtocolConflictingTypeName {}
