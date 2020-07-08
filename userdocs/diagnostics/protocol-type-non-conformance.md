# Protocol type not conforming to itself
Protocols in Swift may be used as types. Protocols as types are sometimes called existential types.


```swift 
protocol P {}

struct S: P {}

var s: P = S() // This creates existential type because the protocol P is used as a type
```

However, a protocol type does not conform to protocols - not even the protocol itself. 
Allowing existential types to conform to protocols is unsound. For protocols with static method, initializer, or associated type requirements, the implementation of these requirements cannot be accessed from the protocol type - accessing these kinds of requirements must be done using a concrete type.

Let's walk through the example below:

```swift
protocol Word: Hashable {
    var word: String { get }
}

struct Singular: Word {
    var word: String
}

struct Plural: Word {
    var word: String
}

let singularWord = Singular(word: "mango")
let pluralWord = Plural(word: "mangoes")

let wordPairDict: [Word: Word] = [singularWord: pluralWord] // Error
```

One workaround to fix this problem is to use type erasure for the protocol `Word`. Think of type erasure as a way to hide an object's type. Since `Word` is of type `Hashable`, we already have `AnyHashable` type erasure available in the standard library which we can easily use here.

```swift 
// The fix
let wordPairDict: [AnyHashable: AnyHashable] = [singularWord: pluralWord]
```

# Exceptions
`@objc` protocol type with no static requirements however do conform to its own protocol. Another exception is the `Error` Swift protocol.

