# `DeclarationUnavailableFromAsynchronousContext`

This diagnostic group includes errors and warnings produced by the compiler when attempting to call a function from an asynchronous context that is marked as unavailable in an asynchronous context.

```swift
@available(*, noasync)
func fNoAsync() {}

// Swift 6 language mode
func test() async {
  fNoAsync() // error: global function 'fNoAsync' is unavailable from asynchronous contexts
}

// Swift 5 language mode
func test() async {
  fNoAsync() // warning: global function 'fNoAsync' is unavailable from asynchronous contexts; this is an error in the Swift 6 language mode
}
```
