# Implicit Copy On Return (ReturnTypeImplicitCopy)

## Overview

Returning `Array` or `Dictionary` values from functions and closures is a common
pattern in Swift. However, these collection types are backed by heap-allocated
storage, and returning them from a function produces an implicit copy of the
collection value.

## Problem

When a function returns an `Array` or a `Dictionary`, it must first construct
the collection in heap-allocated backing storage. The caller then receives a
copy of the collection value - a wrapper struct that contains a pointer to that
storage. Although no element-by-element deep copy occurs at the return boundary
(Swift's collections use copy-on-write to defer that until mutation), the heap
allocation itself is unavoidable, and the copy of the wrapper value carries its
own costs.

1. ***Heap Allocation***: The called function must heap-allocate backing storage
   for the collection it constructs. This allocation cost is paid on every call,
   and the storage is deallocated once the *caller* is done with the result.

2. ***Reference Counting***: The backing storage is reference-counted. Passing
   the wrapper value to the caller involves retain/release operations, and when
   the caller's copy goes out of scope, the storage must be released and
   potentially deallocated.

These costs are especially significant in performance-critical *loops*, where
calling a function that returns a collection on every iteration leads to
repeated allocations and reference counting overhead that can dominate execution
time. The compiler is often unable to eliminate these costs because the returned
collection may be mutated or escape in ways that prevent the optimizer from
proving the copy is unnecessary.

```swift
// Each call allocates and returns a new array
func getVisibleItems() -> [Item] {
  // ... computes and returns a collection of items
}

// In a performance-critical loop, the costs of this operation are incurred on
// every iteration.
for frame in frames {
  let items = getVisibleItems() // allocation + copy on each iteration
  render(items)
}
```

## Alternatives

### 1. Use `inout` parameters

Pass the collection as an `inout` parameter instead of returning it. This allows
the function to populate the caller's collection directly, avoiding the need to
allocate and return a new value.

```swift
func getVisibleItems(_ result: inout [Item]) {
  // ... appends into the caller's collection
}

var items: [Item] = []
getVisibleItems(&items)
```

When used in a loop, the caller can reuse the same collection across iterations
by clearing and refilling it, avoiding the need for repeated heap allocations
entirely.

```swift
var items: [Item] = []
for frame in frames {
  items.removeAll(keepingCapacity: true)
  getVisibleItems(&items)
  render(items)
}
```

### 2. Use `Span` for read-only access

When the caller only needs to read the collection's elements without taking
ownership, consider exposing the data as a
[`Span`](https://developer.apple.com/documentation/swift/span). A `Span` is a
non-escapable view into contiguous memory. The compiler guarantees that a `Span`
cannot outlive the view data so its usage incurs no additional heap allocation
and reference counting overhead. The `Span` value itself is a lightweight data
structure with a pointer and a count, making it cheap to create and pass around.

This approach works well when the data the function is returning is already
stored somewhere with a known lifetime. Instead of returning a copy of the
collection, expose a `Span` that presents a view of the existing storage:

```swift
struct Scene {
  var visibleItems: [Item]
  var visibleItemSpan: Span<Item> {
    self.visibleItems.span
  }
}

for frame in frames {
  let items = scene.visibleItemSpan // no allocation, no copy
  render(items)
}
```

### 3. Restructure to avoid returning transient collections

In some cases, a function builds a collection only for the caller to immediately
process and discard it. The collection itself is transient so the caller does
not need to own a standalone copy of it. This pattern can be restructured so
that the function passes the data to a caller-provided closure instead of
returning it, avoiding the allocation entirely.

For example, consider a `pop` operation on a stack type, which would naturally
return an array:

```swift
// Allocates a new array on every call
mutating func pop(count: Int) -> [Element] {
  let result = Array(items.suffix(count))
  items.removeLast(count)
  return result
}
```

Instead, the function can grant the caller temporary read-only access to the
elements via a closure, then remove them afterward. No intermediate collection
is allocated:

```swift
// Passes a view of the popped elements to the closure, then removes them
mutating func pop<T>(
  count: Int,
  _ body: (borrowing Span<Element>) -> T
) -> T {
  defer { items.removeLast(count) }
  return body(items.span.extracting(last: count))
}
```

The caller processes the elements through the closure without ever owning a
separate collection:

```swift
stack.pop(count: n) { elements in
  for i in elements.indices {
    process(elements[i])
  }
}
```

## Summary

Returning `Array` or `Dictionary` values from functions involves heap allocation
and reference counting overhead that can accumulate in performance-critical code
paths. Use `inout` parameters to allow the caller to provide and reuse storage,
`Span` for non-owning read-only access, or restructure the code to avoid
materializing intermediate collections. These alternatives eliminate the implicit
copies that occur when returning collection values.
