# Using Imported C Structs and Unions in Swift

Learn how Swift represents imported C structures and unions, including types with
bitfields and unnamed fields.

## Overview

Swift imports any C structure declared in a C header as a Swift structure. The imported
Swift structure contains a stored property for each C structure field and an initializer
whose parameters correspond to the stored properties.

### Structures with Default Values

If all imported members have default values, Swift also provides a default initializer
that takes no arguments. For example, given the following C structure:

```occ
struct Color {
    float r, g, b;
};
typedef struct Color Color;
```

When you import the Color structure, the Swift version is equivalent to the following:

```swift
public struct Color {
    var r: Float
    var g: Float
    var b: Float

    init()
    init(r: Float, g: Float, b: Float)
}
```

### Unions

Swift imports C unions as Swift structures. Although Swift doesn’t support natively
declared unions, a C union imported as a Swift structure still behaves like a C union.
For example, consider a C union named `SchroedingersCat` that has an `isAlive` and
an `isDead` field:

```occ
union SchroedingersCat {
    bool isAlive;
    bool isDead;
};
```

In Swift, it’s imported like this:

```swift
struct SchroedingersCat {
    var isAlive: Bool { get set }
    var isDead: Bool { get set }

    init(isAlive: Bool)
    init(isDead: Bool)

    init()
}
```

Because unions in C use the same base memory address for all of their fields, all
of the computed properties in a union imported by Swift use the same underlying memory.
As a result, changing the value of a property on an instance of the imported structure
changes the value of all other properties defined by that structure.

In the example below, changing the value of the `isAlive` computed property on an
instance of the `SchroedingersCat` structure also changes the value of the instance’s
`isDead` computed property:

```swift
var mittens = SchroedingersCat(isAlive: false)

print(mittens.isAlive, mittens.isDead)
// Prints "false false"

mittens.isAlive = true
print(mittens.isDead)
// Prints "true"
```

### Bit Fields

Swift imports bit fields that are declared in structures, like those found in Foundation’s
`NSDecimal` type, as computed properties. When accessing a computed property corresponding
to a bit field, Swift automatically converts the value to and from compatible Swift
types.

### Unnamed Structure and Union Fields

C `struct` and `union` types can define fields that have no name or that are of an
unnamed type. Unnamed fields consist of a nested `struct` or `union` type with named
fields.

For example, consider a C structure named `Cake` that contains the fields `layers`
and `height` nested within an unnamed union type, and a field `toppings` of an unnamed
struct type:

```occ
struct Cake {
    union {
        int layers;
        double height;
    };

    struct {
        bool icing;
        bool sprinkles;
    } toppings;
};
```

After the `Cake` structure has been imported, you can use the default initializer
to create an instance and use it as follows:

```swift
var simpleCake = Cake()
simpleCake.layers = 5
print(simpleCake.toppings.icing)
// Prints "false"
```

The imported `Cake` structure and its nested types are imported with a memberwise
initializer that you can use to initialize the structure with custom values for its
fields:

```swift
let cake = Cake(
    .init(layers: 2),
    toppings: .init(icing: true, sprinkles: false)
)

print("The cake has \(cake.layers) layers.")
// Prints "The cake has 2 layers."
print("Does it have sprinkles?", cake.toppings.sprinkles ? "Yes." : "No.")
// Prints "Does it have sprinkles? No."
```

Because the first field of the `Cake` structure is unnamed, its initializer’s first
parameter doesn't have a label. Because the `Cake` structure has fields with unnamed
types, you use the `.init` initializers (allowed due to Swift's type inference) to
set the initial value for each of the structure’s unnamed fields.
