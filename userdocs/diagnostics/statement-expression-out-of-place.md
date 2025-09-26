# Statement may only be used as an expression in return, throw, or as the source of an assignment

Using an `if` or `switch` statement as an expression is only valid in certain contexts. Resolve this error by breaking up the expression or rewriting it into multiple statements.

An `if` or `switch` statement is allowed to be used as an expression:

* As the value of a return statement. For example:

```swift
func halveIfEven(value: Int) -> Int {
    return if value % 2 == 0 {
        value / 2
    } else {
        value
    }
}
```

* As the value of a `throw` statement. For example:

```swift
func throwError(for errorCode: Int) throws {
    throw switch errorCode {
        case 1: MyError.foo
        case 2: MyError.bar
        default: MyError.default
    }
}
```

* As the value assigned to a variable. For example:

```swift
let result = if condition {
    "hello"
} else {
    "goodbye"
}
```

In any other context, using `if` or `switch` as an expression is not allowed. For example, the following code is invalid because  the`if` statement is used as as an operand of a binary operator expression:

```swift
// error: 'if' may only be used as expression in return, throw, or as the source of an assignment
let result = if condition1 { 1 } else { 2 } + 1
```

This error can be fixed by breaking the code up into multiple statements:

```swift
let x = if condition1 { 1 } else { 2 }
let result = x + 1
```