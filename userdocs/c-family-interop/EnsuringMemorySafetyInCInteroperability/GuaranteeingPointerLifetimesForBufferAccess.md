# Guaranteeing Pointer Lifetimes for Buffer Access

Even after linking a pointer to its count, Swift must still treat the resulting buffer pointer as "unsafe" because it doesn't know the pointer's lifetime. The C function could potentially store the pointer somewhere to be used after the function returns—a situation known as the pointer "escaping".

By using the `noescape` attribute, you can guarantee to the Swift compiler that the pointer is only used for the duration of the function call. This crucial semantic information allows Swift to generate a truly safe API overload that can be called without any `unsafe` constructs.

//FIXME: <explaination of issue>
// According to safe-interop.md, bounds safety features require the experimental feature flag
// `-enable-experimental-feature SafeInteropWrappers` in Swift 6.2

> **Note:** The features described in this section require the experimental feature flag `-enable-experimental-feature SafeInteropWrappers` to be passed to the Swift compiler in Swift 6.2.

### The C Starting Point

Building on the previous example, we start with the `wgpuQueueWriteBuffer` function, already annotated with `__counted_by`. We know from the function's contract that it writes data from the buffer and does not store the pointer.

```c
// In webgpu.h
void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, 
                          uint64_t bufferOffset, 
                          const void* __attribute__((__counted_by__(size))) data, 
                          size_t size);
```

### Initial Swift Import (with `__counted_by`)

The generated overload accepts an `UnsafeRawBufferPointer`. The "unsafe" part of the name exists because Swift cannot prove that the pointer doesn't escape the function call.

```swift
// Swift Interface with bounds safety
extension WGPUQueue {
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: UnsafeRawBufferPointer)
}
```

### Refinement with C Annotations

You add the `noescape` attribute to the `data` parameter. This makes a strong promise to the compiler: the C function will not cause the pointer to outlive the function call.

```c
// In webgpu.h
#include <ptrcheck.h>  // Required for bounds safety macros

void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, 
                          uint64_t bufferOffset, 
                          const void* __counted_by(size) __attribute__((noescape)) data, 
                          size_t size);
```

### Idiomatic Swift Result

With the `noescape` guarantee, Swift can now generate a fully safe overload. The parameter type changes from an `Unsafe...` type to `RawSpan`. This is a huge improvement, as `Span` is a safe buffer type that can be created directly from Swift collections like `Array` without requiring a `withUnsafe...` closure.

```swift
// Resulting Swift Interface
extension WGPUQueue {
    // The previous overloads may still exist, but a new, completely safe one is added.
    //FIXME: <explaination of issue>
    // According to safe-interop.md, void pointers with noescape should use RawSpan, not MutableSpan<UInt8>
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: RawSpan)
}

// Example Truly Safe and Simple Usage
var myData = [UInt8](repeating: 0, count: 1024)
// You can now pass a slice of the array directly.
// No 'unsafe' keywords are needed.
//FIXME: <explaination of issue>
// The syntax for creating RawSpan from an array slice needs to be corrected
myQueue.writeBuffer(buffer: myBuffer, bufferOffset: 0, data: RawSpan(myData[...]))
```

### Achieving the Same with API Notes

To add the `noescape` promise via API Notes, you simply add the `NoEscape: true` key to the parameter's definition.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuQueueWriteBuffer
    SwiftName: "WGPUQueue.writeBuffer(self:buffer:bufferOffset:data:size:)"
    Parameters:
      - Position: 3
        BoundsSafety:
          Kind: counted_by
          BoundedBy: "size"
        # Add the noescape guarantee
        NoEscape: true
