# Deprecated declaration warnings (DeprecatedDeclaration)

Warnings related to deprecated APIs that may be removed in future versions and should be replaced with more current alternatives.

## Overview

The `DeprecatedDeclaration` group covers the following warnings:
- Use of a function annotated with `@available(<platform>, deprecated: <version>)`
  ```swift
  @available(iOS, deprecated: 10.0)
  func oldFunction() {
    // This function is deprecated and should not be used.
  }

  oldFunction() // 'oldFunction()' is deprecated
  ```
- Use of a function annotated with `@available(<platform>, deprecated: <version>, renamed: "<new name>")`
  ```swift
  @available(iOS, deprecated: 10.0, renamed: "newFunction")
  func oldFunction() {
    // This function is deprecated and should not be used.
  }

  oldFunction() // 'oldFunction()' is deprecated: renamed to 'newFunction'
  ```
- Use of a type as an instance of a protocol when the type's conformance to the protocol is marked as deprecated
  ```swift
  struct S {}

  protocol P {}

  @available(*, deprecated)
  extension S: P {}

  func f(_ p: some P) {}

  func test() {
    f(S()) // Conformance of 'S' to 'P' is deprecated
  }
  ```
- When a protocol requirement has a default implementation marked as `deprecated` and the type conforming to the protocol doesn't provide that requirement
  ```swift
  protocol P {
    func f()
    func g()
  }

  extension P {
    @available(*, deprecated)
    func f() {}
    @available(*, deprecated, message: "write it yourself")
    func g() {}
  }

  struct S: P {} // deprecated default implementation is used to satisfy instance method 'f()' required by protocol 'P'
                // deprecated default implementation is used to satisfy instance method 'g()' required by protocol 'P': write it yourself
  ```
- When a protocol requirement has been deprecated
  ```swift
  struct S: Hashable {
    var hashValue: Int { // 'Hashable.hashValue' is deprecated as a protocol requirement; conform type 'S' to 'Hashable' by implementing 'hash(into:)' instead
      ...
    }
  }
  final class C: Executor {
    func enqueue(_ job: __owned Job) {} // 'Executor.enqueue(Job)' is deprecated as a protocol requirement; conform type 'C' to 'Executor' by implementing 'func enqueue(ExecutorJob)' instead
  }
  ```
