# Multiple Inheritance of Classes

In some programming languages, a class can inherit the interface of multiple base classes. Known as multiple inheritance, this feature can add significant complexity to the language and is unsupported in Swift. Instead, Swift allows composition of interfaces using protocols.

Consider the following example with a protocol `Utensil` and various conforming types of specific utensils:

```swift
protocol Utensil { 
    var name: String {get set}
} 


protocol ServingUtensil: Utensil {
    func serve()
} 

extension ServingUtensil {
    func serve() {
        print("Serving food to guests...")
    }
}


protocol Fork: Utensil {
    func spear()
}

extension Fork {
    func spear() { 
        print("Spearing food...")
    }
}


struct CarvingFork: ServingUtensil, Fork {
    var name = "Carving Fork"
}
```

Swift protocols can declare interfaces that must be implemented by each conforming type (like abstract class members in other programming languages such as C# or Java), and they can also provide overridable default implementations for those requirements in protocol extensions.

When class inheritance and protocol conformances are used together, subclasses inherit protocol conformances from base classes, introducing additional complexity. For example, the default implementation of a protocol requirement not overridden in the conforming base class also cannot be overridden in any subclass ([SR-103](https://bugs.swift.org/browse/SR-103)).

To learn more about defining and adopting protocols, see the [Protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html) section in _The Swift Programming Language_. To learn more about class inheritance, see the [Inheritance](https://docs.swift.org/swift-book/LanguageGuide/Inheritance.html) section in _The Swift Programming Language_.
