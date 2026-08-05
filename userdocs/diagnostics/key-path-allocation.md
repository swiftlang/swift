# Key path allocation (KeyPathAllocation)

In Embedded Swift, most key paths are emitted as static objects. However,
certain patterns require the key path to be
allocated on the heap. For example, capturing a value:

```swift
func getPath<T>(index: Int) -> KeyPath<[T], T> {
  return \[T].[index] // a key path that captures 1 value requires a heap allocation
}
```
