# Annotating Pointers and Return Values

In C, any pointer can be `NULL`. When Swift imports C headers without explicit nullability information, it must be conservative and assume any pointer could be null. It represents this by importing pointer types as **Implicitly Unwrapped Optionals** (e.g., `Type!`).

This is unsafe because it shifts the responsibility of handling `nil` to you at runtime. A much better approach is to annotate your C code with `_Nonnull` and `_Nullable` to create a truly optional-safe Swift interface.

### The C Starting Point

Consider the `wgpuInstanceRelease` and `wgpuCreateInstance` functions. The `wgpuInstanceRelease` function should never be passed a null instance, while `wgpuCreateInstance` can return `NULL` if the instance cannot be created.

```c
// In webgpu.h
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);
void wgpuInstanceRelease(WGPUInstance instance);
```

### Initial Swift Import

Without annotations, both the parameter and the return value are imported as implicitly unwrapped optionals. This fails to communicate the API's contract and can lead to runtime crashes if a `nil` value is force-unwrapped.

```swift
// Default Refined Swift Interface
public func wgpuInstanceRelease(_ instance: WGPUInstance!)
public init!(descriptor: UnsafePointer<WGPUInstanceDescriptor>!) -> WGPUInstance!
```

### Refinement with C Annotations

You use `_Nonnull` to guarantee that a pointer will never be null, and `_Nullable` to indicate that a pointer may be null.

```c
// In webgpu.h
// The created instance can be null if creation fails.
_Nullable WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* _Nonnull descriptor);

// The instance passed to release must not be null.
void wgpuInstanceRelease(WGPUInstance _Nonnull instance);
```

// FIXME: Dont reference "NS_ASSUME_NONNULL_BEGIN" which come from foundation, use the real clang attributes instead.

> Note: For convenience, you can also use the macros `NS_ASSUME_NONNULL_BEGIN` and `NS_ASSUME_NONNULL_END` to mark all pointers in a region as non-null by default. You will learn more about this in "Auditing Legacy Code with Assumed Non-null".

### Idiomatic Swift Result

The Swift interface now accurately reflects the C API's contract. The `instance` parameter is non-optional, enforcing the requirement at compile time. The initializer's return type is a proper optional (`?`), forcing you to safely unwrap it.

```swift
// Resulting Swift Interface
public func wgpuInstanceRelease(_ instance: WGPUInstance)
//FIXME: <explaination of issue>
// Initializers don't have return types in their signature - this is a syntax error
public init?(descriptor: UnsafePointer<WGPUInstanceDescriptor>)

// Example Safe Usage
if let myInstance = WGPUInstance(descriptor: &descriptor) {
    // myInstance is guaranteed to be non-nil inside this block.
    wgpuInstanceRelease(myInstance)
}
```

### Achieving the Same with API Notes

You can specify nullability in API Notes using the `Nullable` key for a parameter or return value. To mark an item as non-null, set its `Nullability` to `N`. For nullable, set it to `S` (for single-pointer nullability).

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuCreateInstance
    # Previous SwiftName refinement
    SwiftName: "WGPUInstance.init(descriptor:)"
    Nullability: S # Return value is nullable
    Parameters:
      - Position: 0
        Nullability: N # 'descriptor' parameter is non-null
  - Name: wgpuInstanceRelease
    Parameters:
      - Position: 0
        Nullability: N # 'instance' parameter is non-null
```
