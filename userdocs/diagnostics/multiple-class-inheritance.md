
# Multiple Inheritance of Classes
Unlike traditionally object oriented languages such as C++, Swift does not support multiple inheritance. Instead, Swift is a protocol oriented language that while offering all the features of multiple inheritance, does so with more code readability, safety, and efficiency.

For example, the following code will throw a compile time error when class `Bird` attempts to inherit from both classes `Animal` and `Flyable`.

```swift
class Animal {
    func move() {
        fatalError("Must be implemented by child class.")
    }
}

class Flyable {
    func fly() {
        fatalError("Must be implemented by child class.")
    }
}

class Bird: Animal, Flyable {}
                   // ^ error: multiple inheritance from classes 'Animal' and 'Flying'
```
However, by a slight change of syntax, Swift can support powerful code abstraction through the use of protocols.

In this example, the protocol `Animal` and `Flyable` defines blueprints of methods like `move()` that must be implemented by an adopting class, structure, or enumeration. The class `Bird` now provide concrete implementation to its conforming protocols, and can be described by it's protocol type `Animal` or `Flyable` when such abstraction is needed.

```swift
protocol Animal {
    func move()
}

protocol Flyable {
    func fly()
}

class Bird: Animal, Flyable {
    func move() {
        print("Moving...")
    }
    
    func fly() {
        print("Flying")
    }
}

```
A common pattern that the above example did not demonstrate is when certain functionalities and characteristics are declared in a base class, and a sub class inherit those functionalities and characteristics for free. Swift provide a powerful way  for protocols to implement its own requirements through an extension, and these implementations are available to conforming types by default.

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

protocol Flyable {}

class Bird: Animal, Flyable {}
```
With little surpise, simply call the `move()` method on `Bird` and the default implementation will be used.
```swift
Bird().move()
```

However, conforming types can also provide their own implementations of protocol properties, methods, and initializers. These implementations do not need to contain the `override` keyword or any other special syntax.

```swift
class Bird: Animal, Flyable {
    func move() {
        print("Moving by flying...")
    }
}
```

To learn more about defining and adopting protocols as well as inheritance and subclassing, see the [Protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html) section and the [Inheritance](https://docs.swift.org/swift-book/LanguageGuide/Inheritance.html) section in _The Swift Programming Language_.
