# Multiple Inheritance of Classes

In some programming languages, a class can inherit the interface of multiple base classes. Known as multiple inheritance, this feature can add significant complexity to the language and is unsupported in Swift. Instead, Swift allows composition of interfaces using protocols.

Consider the following example with a class `Bird` conforming to two protocols `Animal` and `Flyable`.

```swift
protocol Flyable {
    func fly()
}

extension Animal {
    func move() {
        print("Moving...")
    }
}

protocol Animal {
    func move()
}

extension Flyable {
    func fly() {
        print("Flying...")
    }
}

class Bird: Animal, Flyable {
    // move() and fly() use default implementations
}
```

Since `Animal` and `Flyable` provide default implementations for their protocol requirements, `Bird` can conform to both without implementing `fly()` or `move()` and the default implementation will be used. This effect is similar to class inheritance where a subclass inherits properties, subscripts and methods from a superclass. The default implementation can be overriden by defining `fly()` or `move()` without an `override` keyword.

When class inheritance and protocol conformances are used together, subclasses inherit protocol conformances from base classes, introducing additional complexity. For example, the default implementation of a protocol requirement not overridden in the conforming base class also cannot be overridden in any subclass ([SR-103](https://bugs.swift.org/browse/SR-103)).

To learn more about defining and adopting protocols as well as inheritance and subclassing, see the [Protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html) section and the [Inheritance](https://docs.swift.org/swift-book/LanguageGuide/Inheritance.html) section in _The Swift Programming Language_.
