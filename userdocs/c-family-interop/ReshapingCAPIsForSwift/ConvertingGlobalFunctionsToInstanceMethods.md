# Converting Global Functions to Instance Methods

A common pattern in C is to pass an opaque handle or pointer as the first argument to every function that operates on it. In Swift, a more natural and object-oriented approach is to represent these operations as instance methods on a type.

You can use the `swift_name` attribute to transform a global C function into an instance method in Swift. This makes your code clearer, more discoverable, and easier to read.

### The C Starting Point

The `wgpuQueueWriteBuffer` function in `webgpu.h` takes a `WGPUQueue` as its first parameter.

```c
// In webgpu.h
void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
                          uint64_t bufferOffset, const void* data, size_t size);
```

### Initial Swift Import

By default, Swift imports this as a global function with unlabeled parameters. The call site is functional but not very descriptive.

```swift
// Default Swift Interface
public func wgpuQueueWriteBuffer(_ queue: WGPUQueue!, _ buffer: WGPUBuffer!, _ bufferOffset: UInt64, _ data: UnsafeRawPointer!, _ size: Int)

// Example Call Site
wgpuQueueWriteBuffer(myQueue, myBuffer, 0, myData, 1024)
```

### Refinement with C Annotations

You use the `swift_name` attribute to map the C function to a Swift instance method. You provide a full Swift method signature, including the type, method name, and argument labels. The special `self` label indicates which C parameter corresponds to the instance.

```c
// In webgpu.h
void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
                          uint64_t bufferOffset, const void* data, size_t size)
    __attribute__((swift_name("WGPUQueue.writeBuffer(self:buffer:bufferOffset:data:size:)")));
```

This attribute maps:
- The function to a method on the `WGPUQueue` type.
- The C parameter `queue` to `self`.
- The C parameter `buffer` to a Swift parameter with the label `buffer`.
- The C parameter `bufferOffset` to a Swift parameter with the label `bufferOffset`.
- The C parameter `data` to a Swift parameter with the label `data`.
- The C parameter `size` to a Swift parameter with the label `size`.

### Idiomatic Swift Result

The function now appears in Swift as an instance method on `WGPUQueue`. The call site is clearer and follows Swift's object-oriented conventions.

```swift
// Resulting Swift Interface
extension WGPUQueue {
    public func writeBuffer(buffer: WGPUBuffer!, bufferOffset: UInt64, data: UnsafeRawPointer!, size: Int)
}

// Example Call Site
myQueue.writeBuffer(buffer: myBuffer, bufferOffset: 0, data: myData, size: 1024)
```

### Achieving the Same with API Notes

If you cannot modify the C header, you can achieve the identical result by adding the following to your `WebGPU.apinotes` file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuQueueWriteBuffer
    SwiftName: "WGPUQueue.writeBuffer(self:buffer:bufferOffset:data:size:)"
```
