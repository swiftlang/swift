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
