# Swift Type Representation In C++

This document describes in details how Swift types are represented in C++.
It also covers related topics, like debug info representation for Swift types in
C++.

## Type Categories

### Value Types

1) Primitive Swift types like `Int`, `Float` , `OpaquePointer`, `UnsafePointer<int>?` are mapped to primitive C++ types. `int` , `float`, `void *`, `int * _Nullable`.

* Debug info: Does C++ debug info suffices?

2) Non-resilient fixed-layout Swift value type, e.g. `String` is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class swift::String {
  ...
  alignas(8) char buffer[24]; // Swift value is stored here.
}
```

* Debug info: ...


3) Resilient (or opaque layout) inline-allocated Swift value type small enough to fit into inline buffer. e.g `URL` is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class Foundation::URL {
  ...
  uintptr_t pointer;         // pointer has alignment to compute buffer offset?
  alignas(N) char buffer[M]; // Swift value is stored here.
};
```

concrete examples:

```c++
// representation for buffer aligned at 8:
{/*pointer=*/0x3, ....}; // buffer is alignas(2^3 = 8)

// representation for buffer aligned at 16:
{/*pointer=*/0x4, ....}; // buffer is alignas(2^4 = 16)

// where pointer < 10 for inline stores.
```

* Debug info: ...


4) Resilient (or opaque layout) boxed Swift value , e.g. `SHA256` is mapped to a C++ class that stores the value boxed up on the heap, e.g.:

```c++
class CryptoKit::SHA256 {
  ...
  uintptr_t pointer;         // Swift value is stored on the heap pointed by this pointer.
  alignas(8) char buffer[8];
};
```

* Debug info: ...


5) Generic non-resilient fixed-layout Swift value type, e.g. `Array<Int>`, `String?`, is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class swift::Array<swift::Int> {
  ...
  alignas(8) char buffer[8]; // Swift value is stored here.
}
```

* Debug info: ...


6) Generic opaque-layout / resilient / opaque-layout template type params Swift value type, e.g. `SHA256?`, is mapped to a C++ class that stores the value boxed up on the heap, e.g.:

```c++
class swift::Optional<CryptoKit::SHA256> {
  ...
  uintptr_t pointer;        // Swift value is stored on the heap pointed by this pointer.
  alignas(N) char buffer[M];
}
```

* Debug info: ...

### Class Types

Class type is mapped to a C++ class that has a pointer to the underlying Swift instance in the base class:

```c++
class BaseClass {
private:
  void *_opaquePointer; // Swift class instance pointer is stored here.
}; 
class Vehicle: public BaseClass {
public:
}
```

* Debug info: ...

### Existential Types

Error type is mapped to a specific C++ `swift::Error` class that stores the pointer to the error:

```c++
class Error {
private:
  void *_opaquePointer; // Swift error instance pointer is stored here.:
};
```

* Debug info: ...


Existential type. e.g. `any Hashable` maps to a C++ class that stores the opaque existential value (`swift::any<swift::Hashable>`):

```c++
class swift::OpaqueExistential {
  // approximate layout.
  alignas(8) char buffer[8*5]; // opaque existential is stored here (inline or boxed by Swift)
};
class swift::any<swift::Hashable>: public swift::OpaqueExistential {
};
```

* Debug info: ...
