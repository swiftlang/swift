# Clarifying Ownership of Returned Objects

Reference-counted C APIs have strict ownership rules. A "create" or "copy" function typically returns an object with a +1 retain count, meaning the caller "owns" the object and is responsible for a future release.

If you have taught Swift how to manage your C object's memory (as shown on the previous page), you must also tell it about this ownership convention. The `SWIFT_RETURNS_RETAINED` macro informs Swift that a function returns an already-retained object, which prevents ARC from retaining it a second time and causing a memory leak.

### The C Starting Point

The `wgpuCreateInstance` function follows the common C convention of returning a new object that the caller is expected to own.

```c
// In webgpu.h
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);
```

### Initial Swift Import

Without ownership information, when you call the `WGPUInstance` initializer, ARC receives the new object and, by default, retains it to claim its own ownership. Since `wgpuCreateInstance` *already* returned a retained object, the instance now has a retain count of +2. When the Swift variable goes out of scope, ARC only releases it once, leading to a memory leak.

### Refinement with C Annotations

You apply the `SWIFT_RETURNS_RETAINED` macro to the `wgpuCreateInstance` function declaration. This signals to Swift that the returned object's retain count has already been incremented.

```c
// In webgpu.h
#include <swift/bridging.h>

WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor) SWIFT_RETURNS_RETAINED;
```

### Idiomatic Swift Result

Swift now understands that the object returned by the `WGPUInstance` initializer does not need an additional retain. ARC correctly manages the object's lifetime, releasing it exactly once when it is no longer needed.

```swift
// Resulting Swift Interface (signature is unchanged, but behavior is corrected)
extension WGPUInstance {
    public init!(descriptor: UnsafePointer<WGPUInstanceDescriptor>!)
}

// Correct Memory Management
func useInstanceCorrectly() {
    // wgpuCreateInstance returns an object at +1.
    // ARC takes ownership without an additional retain.
    let myInstance = WGPUInstance(descriptor: &descriptor)

} // myInstance goes out of scope, ARC calls release once. The object is freed.
```

### Achieving the Same with API Notes

To specify the ownership rule using API Notes, you add the `SwiftReturnOwnership` key to the function's entry in your `.apinotes` file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuCreateInstance
    # Previous SwiftName refinement
    SwiftName: "WGPUInstance.init(descriptor:)"
    # New ownership refinement
    SwiftReturnOwnership: retained
```
