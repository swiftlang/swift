# Creating Safe Buffers from Pointer and Count Pairs

A primary source of memory safety bugs in C is the use of separate parameters for a pointer and its size. The compiler cannot verify that the pointer actually points to a buffer of the specified size.

You can link a pointer parameter to its corresponding count parameter using C attributes like `__counted_by`. When you provide this information, Swift generates a new, safer overload of the function that accepts a single, bounds-checked buffer type, making the API more robust and easier to use.

//FIXME: <explaination of issue>
// According to safe-interop.md, bounds safety features require the experimental feature flag
// `-enable-experimental-feature SafeInteropWrappers` in Swift 6.2

> **Note:** The features described in this section require the experimental feature flag `-enable-experimental-feature SafeInteropWrappers` to be passed to the Swift compiler in Swift 6.2.

### The C Starting Point

The `wgpuQueueWriteBuffer` function takes a `void *` buffer and a `size_t` count as separate arguments.

```c
// In webgpu.h
void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
                          uint64_t bufferOffset, const void* data, size_t size);
```

### Initial Swift Import

The imported Swift method takes an `UnsafeRawPointer` and an `Int`. There is no compile-time guarantee that the buffer passed to the function is large enough to hold `size` bytes, creating a risk of buffer overflows.

```swift
// Default Refined Swift Interface
extension WGPUQueue {
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: UnsafeRawPointer!, size: Int)
}

// Example Unsafe Usage
var myData = [UInt8](repeating: 0, count: 512)
// This call risks reading past the end of myData if the size is wrong.
myQueue.writeBuffer(buffer: myBuffer, bufferOffset: 0, data: &myData, size: 1024)
```

### Refinement with C Annotations

You use the `__counted_by` attribute on the `data` parameter to create a semantic link to the `size` parameter.

//FIXME: <explaination of issue>
// According to safe-interop.md, the ptrcheck.h header should be mentioned for accessing these macros

```c
// In webgpu.h
#include <ptrcheck.h>  // Required for bounds safety macros

void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
                          uint64_t bufferOffset,
                          const void* __counted_by(size) data,
                          size_t size);
```

### Idiomatic Swift Result

With this attribute in place, Swift generates a new, safer overload of the `writeBuffer` method. This overload accepts a single `UnsafeRawBufferPointer`, a type that combines a pointer with its count. This prevents you from accidentally passing mismatched buffer and count values.

```swift
// A new, safe overload is generated
extension WGPUQueue {
    // The original unsafe overload still exists...
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: UnsafeRawPointer!, size: Int)
    // ...but a new, safer overload is now available.
    //FIXME: <explaination of issue>
    // According to safe-interop.md, void pointers should use RawSpan, not UnsafeMutableBufferPointer<Void>
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: UnsafeRawBufferPointer)
}

// Example Safe Usage
var myData = [UInt8](repeating: 0, count: 1024)
myData.withUnsafeBytes { bufferPointer in
    // It's now impossible to pass a size that doesn't match the buffer's size.
    myQueue.writeBuffer(buffer: myBuffer, bufferOffset: 0, data: bufferPointer)
}
```

### Achieving the Same with API Notes

To create this link using API Notes, you add a `BoundsSafety` dictionary to the `data` parameter's definition.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuQueueWriteBuffer
    SwiftName: "WGPUQueue.writeBuffer(self:buffer:bufferOffset:data:size:)"
    Parameters:
      - Position: 3 # The 'data' parameter is at index 3
        BoundsSafety:
          Kind: counted_by
          BoundedBy: "size"
```
