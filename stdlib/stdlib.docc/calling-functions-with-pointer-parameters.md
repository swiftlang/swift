# Calling Functions With Pointer Parameters

Use implicit pointer casting or bridging when calling functions that takes pointers
as parameters.

## Overview

When calling a function that takes a pointer as a parameter, you can use implicit
casting to pass a compatible pointer type or implicit bridging to pass a pointer
to a variable or the contents of an array.

### Pass a Constant Pointer as a Parameter

When you call a function that is declared as taking an `UnsafePointer<Type>` argument,
you can pass any of the following:

- An `UnsafePointer<Type>`, `UnsafeMutablePointer<Type>`, or `AutoreleasingUnsafeMutablePointer<Type>`
value, which is implicitly cast to `UnsafePointer<Type>` as necessary.
- A `String` value, if `Type` is `Int8` or `UInt8`. The string is automatically converted
to UTF8 in a zero-terminated buffer, and a pointer to that buffer is passed to the
function.
- An in-out expression that contains a mutable variable, property, or subscript reference
of type `Type`, which is passed as a pointer to the address of the left-hand side
identifier.
- A `[Type]` value, which is passed as a pointer to the start of the array.

The pointer you pass to the function is only guaranteed to be valid for the duration
of the function call. Do not persist the pointer and access it after the function
has returned.

This example shows the different ways that you can call the a function that takes
a constant pointer:

```swift
func takesAPointer(_ p: UnsafePointer<Float>) {
    // ...
}

var x: Float = 0.0
takesAPointer(&x)
takesAPointer([1.0, 2.0, 3.0])
```

When you call a function that takes an `UnsafeRawPointer` argument, you can pass
the same operands as `UnsafePointer<Type>`, but with any type as `Type`.

This example shows the different ways that you can call a function that takes a constant
raw pointer:

```swift
func takesARawPointer(_ p: UnsafeRawPointer?)  {
    // ...
}

var x: Float = 0.0, y: Int = 0
takesARawPointer(&x)
takesARawPointer(&y)
takesARawPointer([1.0, 2.0, 3.0] as [Float])
let intArray = [1, 2, 3]
takesARawPointer(intArray)
takesARawPointer("How are you today?")
```

### Pass a Mutable Pointer as a Parameter

When you call a function that is declared as taking an `UnsafeMutablePointer<Type>`
argument, you can pass any of the following:

- An `UnsafeMutablePointer<Type>` value.
- An in-out expression of type `Type` that contains a mutable variable, property,
or subscript reference, which is passed as a pointer to the address of the mutable
value.
- An in-out expression of type `[Type]` that contains a mutable variable, property,
or subscript reference, which is passed as a pointer to the start of the array, and
is lifetime-extended for the duration of the call.

This example shows the different ways that you can call a function that takes a mutable
pointer:

```swift
func takesAMutablePointer(_ p: UnsafeMutablePointer<Float>) {
    // ...
}

var x: Float = 0.0
var a: [Float] = [1.0, 2.0, 3.0]
takesAMutablePointer(&x)
takesAMutablePointer(&a)
```

When you call a function that is declared as taking an `UnsafeMutableRawPointer`
argument, you can pass the same operands as `UnsafeMutablePointer<Type>`, but for
any type as `Type`.

This example shows the different ways that you can call a function that takes a mutable
raw pointer:

```swift
func takesAMutableRawPointer(_ p: UnsafeMutableRawPointer?)  {
    // ...
}

var x: Float = 0.0, y: Int = 0
var a: [Float] = [1.0, 2.0, 3.0], b: [Int] = [1, 2, 3]
takesAMutableRawPointer(&x)
takesAMutableRawPointer(&y)
takesAMutableRawPointer(&a)
takesAMutableRawPointer(&b)
```

### Pass an Autoreleasing Pointer as a Parameter

When you call a function that is declared as taking an `AutoreleasingUnsafeMutablePointer<Type>`,
you can pass any of the following:

- An `AutoreleasingUnsafeMutablePointer<Type>` value.
- An in-out expression that contains a mutable variable, property, or subscript reference
of type `Type`. The value of the operand is copied bitwise into a temporary nonowning
buffer. The address of that buffer is passed to the callee, and on return, the value
in the buffer is loaded, retained, and reassigned into the operand.

Unlike with other pointer types, you can't use an array as an implicitly bridged
parameter.

### Pass a Function Pointer as a Parameter

When calling a function that takes a C function pointer argument, you can pass a
top-level Swift function, a closure literal, a closure declared with the `@convention(c)`
attribute, or `nil`. You can also pass a closure property of a generic type or a
generic method as long as no generic type parameters are referenced in the closure’s
argument list or body.

For example, consider Core Foundation’s `CFArrayCreateMutable(_:_:_:)` function.
The `CFArrayCreateMutable(_:_:_:)` function takes a `CFArrayCallBacks` structure,
which is initialized with function pointer callbacks:

```swift
func customCopyDescription(_ p: UnsafeRawPointer?) -> Unmanaged<CFString>? {
    // return an Unmanaged<CFString>? value
}

var callbacks = CFArrayCallBacks(
    version: 0,
    retain: nil,
    release: nil,
    copyDescription: customCopyDescription,
    equal: { (p1, p2) -> DarwinBoolean in
        // return Bool value
    }
)
var mutableArray = CFArrayCreateMutable(nil, 0, &callbacks)
```

In this example, the `CFArrayCallBacks` initializer uses `nil` values as arguments
for the `retain` and `release` parameters, the `customCopyDescription(_:)` function
as the argument for the `customCopyDescription` parameter, and a closure literal
as the argument for the `equal` parameter.

> Note: Only Swift function types with C function reference calling convention may
be used for function pointer arguments. Like a C function pointer, a Swift function
type with the `@convention(c)` attribute does not capture the context of its surrounding
scope.
