# Protocol type not conforming to itself
Swift disallows us from using a protocol as a type that conforms to itself as illustrated in the examples below

```swift
protocol SomeProtocol {
    init()
}

struct SomeStruct: SomeProtocol {}
struct AnotherStruct: SomeProtocol {}

var arr: [SomeProtocol] = [SomeStruct(), AnotherStruct()]
arr.appendNewItem()

extension Array where Element: SomeProtocol {
    mutating func appendNewItem() {
        append(Element())
    }
}
```

The code snippet above would not compile because we are using `SomeProtocol` as a type that conforms to itself. There is no concrete implementation for the protocol.

Consider also the case of using protocol as a type in a generic type - 

```swift
protocol AnotherProtocol {
  static func foo()
}

struct GenericStruct<T: AnotherProtocol> {
    func faz() {
        T.foo()
    }
}

GenericStruct<AnotherProtocol>().faz()
```
Constructing the instance of the struct `GenericStruct` with type `AnotherProtocol` will not compile because there is no concrete implementation for the static requirement of the protocol. 
There is no implementation for for() used above.

