# Handling Nullability in Pointers to Pointers

Some C APIs use multiple levels of pointers, such as `char **`, to pass a pointer by reference. This pattern is often used to return a value that might itself be a pointer, like an error description string.

When importing these types, you must consider the nullability of each pointer level independently. Swift's type system can accurately represent this complexity, but you need to provide the correct annotations.

### The C Starting Point

To demonstrate this, let's add a hypothetical function to WebGPU that retrieves an error description.

```c
// A new function for our case study
// This function returns an error description via an output parameter.
bool wgpuInstanceGetErrorDescription(WGPUInstance instance, const char * _Nullable * _Nonnull outErrorDescription);
```

Here, `const char * _Nullable * _Nonnull` means:
- **`_Nonnull` (outermost):** The `outErrorDescription` parameter itself cannot be `NULL`. You must pass a valid memory address for a `const char *`.
- **`_Nullable` (innermost):** The `const char *` that the function writes to your provided address *can* be `NULL`. This means an error might not have a description.

### Initial Swift Import

Without annotations, Swift would import this as `UnsafeMutablePointer<UnsafePointer<CChar>?>!`. The outer `!` means the parameter itself is an implicitly unwrapped optional, and the inner `?` is inferred from `const`, but it doesn't fully capture the contract.

### Refinement with C Annotations

The declaration shown in "The C Starting Point" is already correctly annotated. By placing `_Nonnull` and `_Nullable` at the appropriate levels, you precisely define the API's contract.

### Idiomatic Swift Result

The Swift interface now correctly represents the two levels of nullability. The parameter type `UnsafeMutablePointer<UnsafePointer<CChar>?>` is not optional itself, but it points to a value that is an optional `UnsafePointer<CChar>`.

```swift
// Resulting Swift Interface
extension WGPUInstance {
    public func getErrorDescription(_ outErrorDescription: UnsafeMutablePointer<UnsafePointer<CChar>?>) -> Bool
}

// Example Usage
var errorStringPtr: UnsafePointer<CChar>? = nil
let success = myInstance.getErrorDescription(&errorStringPtr)

if success, let errorString = errorStringPtr {
    // The C function provided a non-nil error string.
    print(String(cString: errorString))
} else {
    // The C function did not provide an error string.
}
```

### Achieving the Same with API Notes

In API Notes, you can specify nullability for multiple levels of pointers with a string. `N` means non-null, `S` means nullable. The string reads from the outermost pointer inwards. For `const char * _Nullable * _Nonnull`, the nullability is `NS`.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuInstanceGetErrorDescription
    SwiftName: "WGPUInstance.getErrorDescription(self:_:)"
    Parameters:
      # ... self parameter at Position 0
      - Position: 1
        Nullability: NS # Non-null pointer to a nullable pointer
```
