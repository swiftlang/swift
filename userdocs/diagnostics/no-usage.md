# Unused values (NoUsage)

## Overview

These diagnostics are emitted when a value is computed or defined but never used, which often indicates a programming mistake such as a missing assignment or an unintentionally discarded result.

The `NoUsage` group covers a range of unused-value situations like calling a function and ignoring its return value or declaring a variable or constant that is never read.

## Example

```swift
func sortedCopy(_ array: [Int]) -> [Int] {
    var copy = array
    copy.sort()   // ok: sort() mutates in place
    array.sorted() // warning: result of call to 'sorted()' is unused
    return copy
}
```

## How to fix

- Use the result, or assign it to `_` to explicitly discard it:

  ```swift
  _ = incrementGlobalCounter() // increment the counter without using the new value
  ```

- If ignoring the return value of a function is intentional and common for callers, then annotate the function with `@discardableResult`:
  ```swift
  private var counter = 0
  
  @discardableResult // callers may not need to use the new value
  func incrementGlobalCounter() -> Int {
  	counter += 1
    return counter
  }
  ```
  
- Replace an unused binding with `_`:

  ```swift
  enum NetworkError: Error {
    case requestFailed(statusCode: Int, message: String)
    case timeout
  }
  
  func handle(_ error: NetworkError) {
    switch error {
    case .requestFailed(let statusCode, _):  // message not needed here
      print("Request failed with status \(statusCode)")
    case .timeout:
      print("Request timed out")
    }
  }
  ```
