# Branches have mismatching types

Result values in `if` and `switch` expressions must have the same type. Resolve this error by providing a type annotation or adjusting the values to have matching types.

The following code is invalid because one branch of the `if` expression produces a value of type `String`, and the other branch produces a value of type `Int`. The type of result can be either Int or String, but not both.

```swift
let result = if condition {
    "hello, world!" // error: branches have mismatching types 'String' and 'Int'
} else {
    42
}
```

The same restriction applies to each `case` of a `switch` statement used as an expression:

```swift
let result = switch myNumber {
    case 0: "hello, world!" // error: branches have mismatching types 'String' and 'Int'
    case 1: 42
    default: "hello, again!"
}
```

These errors can be resolved by adjusting the code so every branch returns a value of the same type. For example, it would be valid for every branch to produce a `String`:

```swift
let result = if condition {
    "hello, world!"
} else {
    "goodbye!"
}
```

In other situations, it might be appropriate to use an `enum` or `protocol` type to represent the result. Both of these examples would also be valid:

```swift
enum MyEnum {
    case message(String)
    case number(Int)
}

let result = if condition {
    MyEnum.string("hello, world!")
} else {
    MyEnum.number(42)
}
```

```swift
protocol MyProtocol { ... }
extension String: MyProtocol { ... }
extension Int: MyProtocol { ... }

let result = if condition {
    "hello, world!" as (any MyProtocol)
} else {
    42 as (any MyProtocol)
}
```

Because each branch of an `if` or `switch` expression is typechecked independently, the following code is also invalid:

```swift
let result = if condition {
    0 // error: branches have mismatching types 'Int' and 'Double'
} else {
    1.0
}
```

Even though both `0` and `1.0` could represent a value of type `Double`, because each branch of the expression is typechecked independently `0` is inferred to be of type `Int` and `1.0` is inferred to be of type `Double`, causing a mismatch. In this case, an explicit type annotation can be used to ensure both literals are treated as a `Double`:

```swift
let result: Double = if condition {
    0
} else {
    1.0
}
```

```swift
let result = if condition {
    0 as Double
} else {
    1.0
}
```