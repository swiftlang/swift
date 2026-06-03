# Adopting Common Protocols

Make your custom types easier to use by ensuring that they conform to Swift protocols.

## Overview

When using custom types to model data in your programs, you may frequently need to
check whether two values are the same or different, or whether a particular value
is included in a list of values. This capability, as well as the ability to store
values in a set or use them as keys in a dictionary, are governed by two related
standard library protocols, ``Swift/Equatable`` and ``Swift/Hashable``.

- You can compare instances of an _equatable_ type by using the equal-to (`==`) and
not-equal-to (`!=`) operators.
- An instance of a _hashable_ type can reduce its value mathematically to a single
integer, which is used internally by sets and dictionaries to make lookups consistently
fast.

Many standard library types are both equatable and hashable, including strings, integers,
floating-point values, Boolean values, and collections of equatable and hashable
types. The `==` comparison and the `contains(_:)` method call in the following example
depend on strings and integers being equatable:

```swift
if username == "Arturo" {
    print("Hi, Arturo!")
}

let favoriteNumbers = [4, 7, 8, 9]
if favoriteNumbers.contains(todaysDate.day) {
    print("It's a good day today!")
}
```

Conforming to the `Equatable` and `Hashable` protocols is straightforward and makes
it easier to use your own types in Swift. It's a good idea for all your custom model
types to conform.

### Conform Automatically to Equatable and Hashable

You can make many custom types equatable and hashable by simply declaring these protocol
conformances in the same file as the type's original declaration. Add `Equatable`
and `Hashable` to the list of adopted protocols when declaring the type, and the
compiler automatically fills in the requirements for the two protocols:

```swift
/// A position in an x-y coordinate system.
struct Position: Equatable, Hashable {
    var x: Int
    var y: Int

    init(_ x: Int, _ y: Int) {
        self.x = x
        self.y = y
    }
}
```

With `Equatable` conformance, you can use the equal-to operator (`==`) or the not-equal-to
operator (`!=`) with any two instances of the `Position` type.

```swift
let availablePositions = [Position(0, 0), Position(0, 1), Position(1, 0)]
let gemPosition = Position(1, 0)

for position in availablePositions {
    if gemPosition == position {
        print("Gem found at (\(position.x), \(position.y))!")
    } else {
        print("No gem at (\(position.x), \(position.y))")
    }
}
// No gem at (0, 0)
// No gem at (0, 1)
// Gem found at (1, 0)!

```

`Hashable` conformance means that you can store positions in a set and quickly check
whether you've visited a position before, as shown in the following example:

```swift
var visitedPositions: Set = [Position(0, 0), Position(1, 0)]
let currentPosition = Position(1, 3)

if visitedPositions.contains(currentPosition) {
    print("Already visited (\(currentPosition.x), \(currentPosition.y))")
} else {
    print("First time at (\(currentPosition.x), \(currentPosition.y))")
    visitedPositions.insert(currentPosition)
}
// First time at (1, 3)

```

In addition to simplifying your code, this automatic conformance reduces errors,
because any new properties you add to your custom types are automatically included
when hashing and testing for equality. A type is eligible for automatic conformance
to `Equatable` and `Hashable` when it's a structure or an enumeration that meets
these criteria:

- For a structure, _all_ its stored properties must conform to `Equatable` and `Hashable`.
- For an enumeration, _all_ its associated values must conform to `Equatable` and
`Hashable`. (Enumerations without associated values have `Equatable` and `Hashable`
conformance even without declaring adoption.)

### Conform Manually to Equatable and Hashable 

You need to manually implement `Equatable` and `Hashable` conformance for a type
in these cases:

- The type doesn't meet the criteria listed in the previous section.
- You want to customize the type's conformance.
- You want to extend a type declared in another file or module to conform.

```swift
class Player {
    var name: String
    var position: Position

    init(name: String, position: Position) {
        self.name = name
        self.position = position
    }
}
```

The `Player` type is a class, so it doesn't qualify for automatic synthesis of the
`Equatable` or `Hashable` requirements. To make this class conform to the `Equatable`
protocol, declare conformance in an extension and implement the  static `==` operator
method. Compare each significant property for equality in your `==` method's implementation:

```swift
extension Player: Equatable {
    static func ==(lhs: Player, rhs: Player) -> Bool {
        return lhs.name == rhs.name && lhs.position == rhs.position
    }
}
```

To make `Player` conform to the `Hashable` protocol, declare conformance in another
extension and implement the `hash(into:)` method. In the `hash(into:)` method, call
the `combine(_:)` method on the provided hasher with each significant property:

```swift
extension Player: Hashable {
    func hash(into hasher: inout Hasher) {
        hasher.combine(name)
        hasher.combine(position)
    }
}
```

### Use All Significant Properties for Equatable and Hashable

When implementing the `==` method and the `hash(into:)` method, use all the properties
that affect whether two instances of your custom type are considered equal. In the
implementations above, the `Player` type uses `name` and `position` in both methods.

If your type contains properties that don't affect whether two instances are considered
equal, exclude those properties from comparison in the `==` method and from hashing
in `hash(into:)`. For example, a type might cache an expensive computed value so
that it only needs to calculate it once. If you compare two instances of that type,
whether or not the computed value has been cached shouldn't affect their equality,
so the cached value should be excluded from comparison and hashing.

> Important: Always use the same properties in both your `==` and `hash(into:)` methods.
Using different groups of properties in the two methods can lead to unexpected behavior
or performance when using your custom type in sets and dictionaries.
