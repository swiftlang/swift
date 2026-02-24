# Implicit strong captures of weak capture list items (ImplicitStrongCapture)

## Overview

When you use a `weak` or `unowned` capture list item in a closure, but that object is implicitly captured as a strong reference in an outer escaping closure, the compiler emits a warning. This warning highlights cases that can cause unexpected reference cycles or unintended object lifetime extension.

For example, consider a hypothetical `DataService` class that uses a callback-based API to fetch data from the network, store the response model into a cache, and finally notify clients of an update:

```swift
final class DataService: Sendable {
  func fetchData(using cache: Cache) {
    networkService.fetch { responseData in
      let result = DataModel(responseData)
      cache.store(result) { [weak self] in
        self?.notifyClients(with: result)
      }
    }
  }
}
```

Here, `self` might appear to be weakly retained during the call to `fetch`, since it's only captured weakly in the inner closure. However, the presence of the capture list item causes the outer closure to capture a strong reference to `self`. To highlight this, the compiler emits a warning noting the ownership mismatch:

```
| final class DataService: Sendable {
|   func fetchData(using cache: Cache) {
|     networkService.fetch { responseData in
|                          |- note: 'self' implicitly strongly captured here
|                          `- note: add 'self' as a capture list item to silence
|       let result = DataModel(responseData)
|       cache.store(result) { [weak self] in
|                                   |- warning: 'weak' ownership of capture 'self' differs from implicitly-captured strong reference in outer scope
|                                   `- note: explicitly assign the capture list item to silence
|         self?.notifyClients(with: result)
|       }
```

To resolve the warning, add an explicit capture list item to the outer closure specifying the intended ownership. You can also silence the warning by explicitly assigning a value to the inner closure's capture list item (e.g. `[weak x = x]`).
