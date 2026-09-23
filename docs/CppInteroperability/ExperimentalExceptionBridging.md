# Experimental C++ exception bridging

This prototype imports C++ functions annotated with `SWIFT_THROWS` as throwing
Swift functions. It is being developed in the
[Swift Forums discussion](https://forums.swift.org/t/handling-c-exceptions-in-swift-swift-throws-and-throws/89732).
The annotation and behavior are experimental.

Enable C++ interoperability and `-enable-experimental-feature CxxExceptionBridging`.
With a toolchain whose `<swift/bridging>` header provides `SWIFT_THROWS`:

```cpp
#include <swift/bridging>
#include <stdexcept>

SWIFT_THROWS inline int checkedValue(int value) {
  if (value < 0)
    throw std::runtime_error("value must be nonnegative");
  return value;
}
```

Swift calls require `try`, including calls through captured function values:

```swift
import Cxx
import MyCppLibrary

do {
  let value = try checkedValue(-1)
  print(value)
} catch let error as CxxException {
  print(error.message)
}
```

`CxxException.message` contains a Swift-owned copy of `std::exception::what()`.
Invalid UTF-8 is replaced. Other native C++ exception types produce the message
`"Unknown C++ exception"`. The error does not retain the original exception or
expose its C++ type.

A generated C++ adapter catches the exception before returning to Swift. Swift
then throws the copied error through its normal error-handling path. C++ stack
cleanup completes inside the adapter, and Swift `defer` blocks run when the Swift
error propagates. The original C++ function keeps its ABI and symbol.

The current slice supports free functions, namespace functions, and static and
ordinary instance methods with arithmetic or enum parameters and arithmetic or
`void` results. Instance methods preserve their const and reference qualifiers,
mutating behavior, and existing C++ virtual dispatch rules, including calls to
`super` on foreign reference types. Mutations made before an exception remain
visible to Swift. Captured method values also throw. Throwing getter and setter
methods do not become nonthrowing computed properties.

Consuming methods require a receiver whose move construction and destruction
cannot throw, and whose copy construction also cannot throw when it is
`Copyable`. These operations may run in Swift-generated code around the adapter.
This permits consuming methods on nontrivial and noncopyable receivers while
keeping throwing implicit lifetime operations outside the supported scope.
Inherited rvalue-qualified methods retain the importer's existing limitations.

Other annotated declarations are unavailable. Constructors, operators, default
arguments, variadic functions, and functions with aggregate or pointer
parameters or results need additional support. Explicit object member functions
are also unsupported.

The annotation takes precedence over `noexcept` for the imported Swift function
type. A C++ violation of `noexcept` still terminates according to C++ rules.
Implicit copies, moves, destructors, and retain/release operations are outside
the recovery boundary. Raw function-pointer calls do not acquire throwing
semantics from this annotation.

The prototype supports Darwin and Linux with C++ exceptions enabled. Darwin
also requires Objective-C interoperability and Objective-C exception handling.
Objective-C exceptions and foreign unwind exceptions terminate instead of
becoming Swift errors. Windows and Embedded Swift are not supported.

A Swift module built with both C++ interoperability and this experimental
feature requires both settings in its consumers. This requirement applies even
when the producer or consumer opts out of the usual C++ interoperability import
requirement. The importer needs C++ interoperability to reconstruct the
exception adapters and their throwing function types from serialized bodies,
including when a module exposes only Swift types in its public API.
