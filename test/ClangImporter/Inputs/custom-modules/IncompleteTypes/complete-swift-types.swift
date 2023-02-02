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
