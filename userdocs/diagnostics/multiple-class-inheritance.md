# Multiple Inheritance of Classes

In some programming languages, a class can have multiple superclasses. This feature is called multiple inheritance and is unsupported in Swift. However, using protocols, one can achieve a similar effect as multiple inheritance.

For example, the protocol `Animal` and `Flyable` defines blueprints of methods like `move()`. The class `Bird` now provide concrete implementation to its conforming protocols.

```swift
protocol Animal {
    func move()
}

class Bird: Animal, Flyable {
    func move() {
        print("Moving...")
    }
}

```
A common pattern is when certain functionalities and characteristics are declared in a base class, and a subclass inherit those functionalities and characteristics for free. Swift provide a powerful way for protocols to implement its own requirements through an extension, and these implementations are available to conforming types by default.

Notice how there is no compiler error thrown when class `Bird` does not provide any implementation for `Animal`'s `move()` method? The extension of `Animal` has already provided a default implementation of `move()` that is available for all conforming types.
  
```swift
protocol Animal {
    func move()
}

extension Animal {
    func move() {
        print("Moving...")
    }
}
```

Conforming types can also provide their own implementations, and these declarations do not need to contain the `override` keyword. When the conforming type is a `class`, note that subclasses of a base class adopting a protocol with default implementations cannot override those declarations without the parent overriding the default implementation. 

To learn more about defining and adopting protocols as well as inheritance and subclassing, see the [Protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html) section and the [Inheritance](https://docs.swift.org/swift-book/LanguageGuide/Inheritance.html) section in _The Swift Programming Language_.
