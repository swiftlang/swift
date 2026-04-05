# Untyped throws (UntypedThrows)

A Swift function can declare that it can throw an error using `throws`, optionally providing a specific thrown error type:

```swift
func canThrowAnything() throws { ... }          // untyped throws
func canThrowMyError() throws(MyError) { ... }  // typed throws (only throws MyError)
```

When a specific thrown error type is omitted, `any Error` will be used instead. Throwing an error of type `any Error` involves a heap allocation as well as reference-counting overhead. Therefore, highly performance-sensitive code should prefer to use typed throws over untyped throws.

## See Also

- [SE-0413: Typed Throws](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0413-typed-throws.md)
