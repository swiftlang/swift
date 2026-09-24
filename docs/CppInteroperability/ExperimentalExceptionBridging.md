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

It also supports constructors of escapable C++ value types with arithmetic or
enum parameters. Their Swift initializers are throwing, including captured
references such as `let makeValue: (CInt) throws -> MyCppType = MyCppType.init`.
Constructors inherited through `using Base::Base` preserve the annotation.

Constructors initialize temporary storage inside the C++ adapter. If construction
fails, C++ destroys the initialized subobjects and Swift leaves the result
storage uninitialized. Successful results are transferred into Swift storage.
These transfers and consuming method receivers require nonthrowing move
construction and destruction; copyable types also require nonthrowing copy
construction. Clang checks the operations that overload resolution selects,
including a copy constructor used for a move expression. Noncopyable values do
not require a copy constructor. Inherited rvalue-qualified methods retain the
importer's existing limitations.

Other annotated declarations are unavailable. Constructors of foreign reference
types, operators, default arguments, variadic functions, and functions with
aggregate or pointer parameters or results need additional support. Explicit
object member functions are also unsupported.

The annotation takes precedence over `noexcept` for the imported Swift function
type. A C++ violation of `noexcept` still terminates according to C++ rules.
Implicit copies, moves, destructors, and retain/release operations are outside
the recovery boundary. The constructor restrictions above prevent exceptions
from the transfers required by this implementation; they do not add recovery
to implicit operations elsewhere in Swift. Raw function-pointer calls do not
acquire throwing semantics from this annotation.

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
