# Transforming Factory Functions into Initializers

In C, it is common to use a "factory" function (often with a "Create" prefix) to allocate and return a new instance of an object. Swift encapsulates this concept in its initializers (`init`). By mapping a C factory function to a Swift initializer, you enable natural, idiomatic object creation.

You can use `swift_name` to import a C function that returns a new object as a native Swift initializer.

### The C Starting Point

The `wgpuCreateInstance` function in `webgpu.h` allocates and returns a new `WGPUInstance`. It returns `NULL` if creation fails.

```c
// In webgpu.h
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);
```

### Initial Swift Import

By default, this is imported as a global function. You create a new instance by calling this function directly.

```swift
// Default Swift Interface
public func wgpuCreateInstance(_ descriptor: UnsafePointer<WGPUInstanceDescriptor>!) -> WGPUInstance

// Example Usage
let myInstance = wgpuCreateInstance(&descriptor)
```

### Refinement with C Annotations

You use the `swift_name` attribute to redefine the factory function as an initializer for the `WGPUInstance` type. The function name `init` is a special keyword that tells the compiler to import the C function as an initializer.

```c
// In webgpu.h
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor)
    __attribute__((swift_name("WGPUInstance.init(descriptor:)")));
```

This attribute maps:
- The function to an `init` on the `WGPUInstance` type.
- The C parameter `descriptor` to a Swift parameter with the label `descriptor`.

Because the original C function is nullable (it can return `NULL`), Swift imports this as a failable initializer (`init!`).

### Idiomatic Swift Result

The C function is now exposed as a native initializer in Swift. This allows you to create instances using standard Swift syntax, which is more intuitive and consistent with other Swift APIs.

```swift
// Resulting Swift Interface
extension WGPUInstance {
    public init!(descriptor: UnsafePointer<WGPUInstanceDescriptor>!)
}

// Example Usage
let myInstance = WGPUInstance(descriptor: &descriptor)
```

### Achieving the Same with API Notes

To apply this refinement without modifying the C header, add the following to your `WebGPU.apinotes` file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuCreateInstance
    SwiftName: "WGPUInstance.init(descriptor:)"
```
