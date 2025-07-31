# Managing the Lifetime of C Objects

Many C APIs vend opaque pointers to objects whose memory must be managed manually. Typically, you call a "create" function to get a pointer and are then responsible for calling a corresponding "release" function when you are done. This manual process is a common source of memory leaks and crashes.

You can teach Swift's Automatic Reference Counting (ARC) system how to manage the lifetime of a C object. By specifying the object's retain and release functions, you can import it as a Swift `class`, letting ARC handle its memory automatically and safely.

> Note: To demonstrate a retain operation, we will assume the existence of a hypothetical `wgpuInstanceAddRef` function. A true reference-counted C API would provide such a function.

### The C Starting Point

In `webgpu.h`, `WGPUInstance` is an opaque pointer. The caller is responsible for balancing a call to `wgpuCreateInstance` with a call to `wgpuInstanceRelease`.

```c
// In webgpu.h
typedef struct WGPUInstanceImpl *WGPUInstance;

// Assume this function increments the instance's reference count.
void wgpuInstanceAddRef(WGPUInstance instance);

// This function decrements the instance's reference count and frees it when the count is zero.
void wgpuInstanceRelease(WGPUInstance instance);
```

### Initial Swift Import

By default, the instance is just a `typealias` for `OpaquePointer`. Swift has no knowledge of its lifecycle, forcing you to call `wgpuInstanceRelease` manually. Forgetting this call results in a memory leak.

```swift
// Default Swift Interface
public typealias WGPUInstance = OpaquePointer?

// Example Manual Management
let myInstance = wgpuCreateInstance(&descriptor)
// ... use myInstance
wgpuInstanceRelease(myInstance) // Manual release required.
```

### Refinement with C Annotations

You use the `SWIFT_SHARED_REFERENCE` macro to tell Swift that `WGPUInstanceImpl` is a reference-counted object. You provide the retain (`wgpuInstanceAddRef`) and release (`wgpuInstanceRelease`) functions as arguments to the macro.

> This attribute is applied to the underlying `struct` declaration, not the `typedef`. If the `struct` is not defined in the header, you would apply this refinement using API Notes.

//FIXME: <explaination of issue>
// According to "Adapting a C header for Swift.md", there are important Swift 6.2 limitations missing
// Swift 6.2 has a compiler bug where SWIFT_SHARED_REFERENCE doesn't work on incomplete types
// Swift 6.2 requires the compiler flags `-Xfrontend -experimental-c-foreign-reference-types`

> **Note:** Swift 6.2 has a compiler bug where `SWIFT_SHARED_REFERENCE` does not work on incomplete types. Having a definition for the struct, even if it's `{ }`, works around the bug. In addition, Swift 6.2 requires the compiler flags `-Xfrontend -experimental-c-foreign-reference-types` when using `SWIFT_SHARED_REFERENCE` with C interoperability.

```c
// In webgpu.h
#include <swift/bridging.h> // Required for the macro

typedef struct SWIFT_SHARED_REFERENCE(wgpuInstanceAddRef, wgpuInstanceRelease) WGPUInstanceImpl *WGPUInstance;
```

### Idiomatic Swift Result

Swift now imports `WGPUInstanceImpl` as a `class`. ARC will now automatically manage the memory for instances of this type. You no longer need to call the release function manually, which eliminates an entire class of potential bugs.

```swift
// Resulting Swift Interface
open class WGPUInstanceImpl { }
public typealias WGPUInstance = WGPUInstanceImpl?

// Example Automatic Management
func useInstance() {
    let myInstance = WGPUInstance(descriptor: &descriptor)
    // ... use myInstance
} // ARC automatically calls wgpuInstanceRelease when myInstance goes out of scope.
```

### Achieving the Same with API Notes

To apply this refinement externally, you add an entry for the `WGPUInstanceImpl` struct in your API Notes file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Tags:
  - Name: WGPUInstanceImpl
    SwiftImportAs: reference
    SwiftRetainOp: wgpuInstanceAddRef
    SwiftReleaseOp: wgpuInstanceRelease
```
