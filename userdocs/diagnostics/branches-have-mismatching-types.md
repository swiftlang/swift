# Branches have mismatching types

When an `if` or `switch` statement is used as an expression, each of its branches must produce a value of the same type when type checked independently.

For example, the following code is invalid because one branch of the `if` expression produces a value of type `String`, and the other branch produces a value of type `Int`. As a result, the compiler is unable to infer a type for `result`.

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

Because each branch of an `if` or `switch` expression is typechecked independently, the following code is also invalid:

```swift
let result = if condition {
    0 // error: branches have mismatching types 'Int' and 'Double'
} else {
    1.0
}
```

Even though both `0` and `1.0` could represent a value of type `Double`, because each branch of the expression is typechecked independently `0` is inferred to be of type `Int` and `1.0` is inferred to be of type `Double`, causing a mismatch.