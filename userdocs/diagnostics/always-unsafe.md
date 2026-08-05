# Always unsafe (AlwaysUnsafe)

## Overview

These errors are emitted when an entity marked `@unsafe(always)` is used without
acknowledging the unsafety with the `unsafe` keyword.

`@unsafe` and `@unsafe(always)` describe two different degrees of unsafety:

- `@unsafe` marks an entity whose *misuse* is not memory-safe, but which has
  reasonable uses that are. Uses only need to be marked with `unsafe` when
  strict memory safety checking is enabled (`-strict-memory-safety`), because
  most code does not need to audit every such use.
- `@unsafe(always)` marks an entity that is very hard to use correctly: nearly
  every use of it is incorrect. Uses always need to be marked with `unsafe`,
  regardless of whether strict memory safety checking is enabled, because
  writing such a use should be a deliberate act.

Marking a use with `unsafe` does not make it correct. It records that you have
established, by other means, that this particular use is correct.

## Example

A C++ API is imported as `@unsafe(always)` when the compiler cannot faithfully
represent the lifetime rules the API expects, so it cannot check that a result
does not outlive the value it borrows from:

```cpp
// Buffer.h
#include <cstdint>
#include <span>
#include <vector>

class Buffer {
public:
  Buffer(size_t count) : storage_(count) {}

  // The returned span points into 'storage_', so it must not outlive 'this'.
  // Nothing here tells Swift about that requirement.
  std::span<const int32_t> elements() const { return storage_; }

private:
  std::vector<int32_t> storage_;
};
```

Because the dependency of the result on `self` was not imported, Swift does not
know it has to keep the `Buffer` alive for as long as the span is used:

```swift
func firstElement(count: Int) -> Int32 {
  let buffer = Buffer(count)
  let elements = buffer.elements()  // error: expression uses constructs that are
                                    // very hard to use correctly and must be
                                    // marked with 'unsafe'
  return elements[0]                // 'buffer' may already have been destroyed
}
```

## How to fix

Consult the documentation of the entity and establish that your use satisfies
its requirements. Here that means keeping the `Buffer` alive across every use of
the span, which `withExtendedLifetime(_:_:)` guarantees. Then acknowledge the
remaining unsafety with `unsafe`:

```swift
func firstElement(count: Int) -> Int32 {
  let buffer = Buffer(count)
  return withExtendedLifetime(buffer) {
    let elements = unsafe buffer.elements()
    return unsafe elements[0]
  }
}
```

Adding `unsafe` on its own would silence the error without making the code
correct — the marker records that you have verified the use, not that the
compiler has.

Where possible, prefer fixing the API instead. Annotating the C++ declaration
with `[[clang::lifetimebound]]` lets Swift import the lifetime dependency, after
which the API is no longer always-unsafe and the compiler enforces the rule for
you:

```cpp
std::span<const int32_t> elements() const [[clang::lifetimebound]] {
  return storage_;
}
```

If you are wrapping the entity in an API that encapsulates the unsafety, mark
the wrapper `@safe` so its callers do not have to repeat the audit.

## See Also

- <doc:strict-memory-safety>
- <doc:unnecessary-unsafe>
