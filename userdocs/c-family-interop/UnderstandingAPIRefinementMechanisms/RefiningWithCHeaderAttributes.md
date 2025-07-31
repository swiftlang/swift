# Refining with C Header Attributes

You can refine how Swift imports a C API by adding attributes directly to the C header files. This inline approach embeds Swift-specific metadata alongside the original C declarations, giving you precise control over the generated Swift interface.

## Overview

Clang, the compiler front end used by Swift, provides a syntax for adding metadata to declarations called attributes. Swift recognizes a number of these attributes and uses them to adjust how it imports C code.

You apply attributes directly in the C source code using the `__attribute__((...))` syntax. For convenience, Swift also provides a set of wrapper macros (like `SWIFT_NAME` or `SWIFT_SHARED_REFERENCE`) that are often easier to read.

This method is ideal when you own the C source code or have the flexibility to modify it.

### How It Works

You place an attribute directly before or after the C declaration you want to influence. For example, to change the name of a C function when it is imported into Swift, you can use the `swift_name` attribute.

Consider the `wgpuDeviceGetQueue` function from our WebGPU case study:

```c
// Original C declaration in webgpu.h
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device);
```

By default, Swift imports this as a global function: `wgpuDeviceGetQueue(device)`. To make this feel more natural in Swift, you want to import it as a computed property named `queue` on the `WGPUDevice` type.

You can achieve this by adding the `swift_name` attribute to the declaration in `webgpu.h`:

```c
// Modified C declaration in webgpu.h
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device)
    __attribute__((swift_name("getter:WGPUDevice.queue(self:)")));
```

This attribute tells the Swift compiler to:
1.  Import this function as a getter (`getter:`).
2.  Make it a member of the `WGPUDevice` type.
3.  Name the resulting property `queue`.
4.  Use the function's first parameter as the `self` instance for the property.

### When to Use Header Attributes

Using C header attributes is a powerful way to provide rich semantic information to the Swift compiler.

**Advantages:**
- **Co-location:** The Swift-specific metadata lives directly with the C declaration it affects, making the relationship clear and easy to maintain.
- **Single Source of Truth:** The header file contains all the information about the declaration for both C and Swift.

**Considerations:**
- **Requires Source Modification:** You can only use this method if you can edit the C header files. This makes it unsuitable for system libraries or pre-compiled third-party libraries.
- **Adds Non-Standard Syntax:** The attributes add syntax to your headers that is specific to the Clang and Swift compilers.

In the following sections, you will see many examples of how different attributes can be used to reshape and secure your C API.
