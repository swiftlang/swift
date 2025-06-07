# Strict memory safety (StrictMemorySafety)

Warnings that identify the use of language constructs and library APIs that can undermine memory
safety, disabled by default.


## Overview

Strict memory safety can be enabled with the `-strict-memory-safety` compiler flag. Examples of
memory-unsafe code in Swift include:

- Use of a function or type annotated with `@unsafe`:
  ```swift
  func getPointee<T>(_ pointer: UnsafeMutablePointer<Int>, as type: T.Type) -> T {
    // reference to unsafe global function 'unsafeBitCast'
    return unsafeBitCast(pointer.pointee, to: type)
  }
  ```
- Use of a function involving an `@unsafe` type:
  ```swift
  func evilMalloc(size: Int) -> Int {
    // use of global function 'malloc' involves unsafe type 'UnsafeMutableRawPointer'
    return Int(bitPattern: malloc(size))
  }
  ```

- Use of an entity that makes use of an unsafe language feature:

  ```swift
  // use of an unowned(unsafe) variable is not memory-safe
  unowned(unsafe) var parentNode: TreeNode<T>
  ```

- Definition of a type whose storage contains unsafe types:
  ```swift
  // struct MyTemporaryBuffer has storage involving unsafe types
  struct MyTemporaryBuffer<T> {
    private var storage: UnsafeBufferPointer<T>
  }
  ```

- Using an `@unsafe` operation to satisfy a safe protocol requirement:
  ```swift
  // conformance of 'MyType' to protocol 'CustomStringConvertible' involves unsafe code
  struct MyType: CustomStringConvertible {
    @unsafe var description: String {
      "I am unsafe!"
    }
  }
  ```

The warnings produced by strict memory safety can be suppressed by acknowledging the unsafe behaior with one of three constructs:

* `unsafe` expressions to acknowledges that there exists memory-unsafe code within the given expression, similar to the way `try` and `await` work for throwing and asynchronous functions:

  ```swift
  func evilMalloc(size: Int) -> Int {
    return unsafe Int(bitPattern: malloc(size))
  }
  ```

* `@unsafe` attribute, which acknowledges that a type or conformance is also unsafe:

  ```swift
  struct MyType: @unsafe CustomStringConvertible {
    @unsafe var description: String {
      "I am unsafe!"
    }
  }
  ```

* `@safe` attribute to indicate that an entity encapsulates the unsafe behavior to provide a safe interface:

  ```swift
  @safe struct MyTemporaryBuffer<T> {
    private var storage: UnsafeBufferPointer<T>
  }
  ```
