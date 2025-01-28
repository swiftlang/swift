# `ExistentialAny`

This diagnostic group includes type syntax errors and warnings that may arise on
occurrences of protocol or protocol composition types without an `any` or `some`
qualifier in type position (vs. constraint position).
For example:

```swift
struct AnyJoinedSequence<Element> {
  private let sequences: [Sequence<Element>] // error.
}

extension AnyJoinedSequence: Sequence, IteratorProtocol {
  func next() -> Element? {
    fatalError("Not implemented")
  }
}
```
