# Mapping Getter Functions to Computed Properties

In C, you often use dedicated functions to retrieve a value associated with an object. In Swift, this pattern is better represented by a computed property. A property feels more lightweight and aligns with Swift's design principle of uniform access.

You can use the `getter:` prefix with the `swift_name` attribute to import a C function as a read-only computed property in Swift.

### The C Starting Point

The `wgpuDeviceGetQueue` function in `webgpu.h` simply returns the queue value for a given device.

```c
// In webgpu.h
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device);
```

### Initial Swift Import

By default, this is imported as a global function. To get the queue, you must call the function and pass the device as an argument.

```swift
// Default Swift Interface
public func wgpuDeviceGetQueue(_ device: WGPUDevice!) -> WGPUQueue

// Example Usage
let queue = wgpuDeviceGetQueue(myDevice)
```

### Refinement with C Annotations

You add the `swift_name` attribute with the `getter:` prefix to the C function declaration. This tells the compiler to import the function as a property getter.

```c
// In webgpu.h
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device)
    __attribute__((swift_name("getter:WGPUDevice.queue(self:)")));
```
This attribute specifies that:
- The function is a `getter:`.
- The property is named `queue` and belongs to the `WGPUDevice` type.
- The C parameter `device` is the instance (`self`) on which the property is accessed.

### Idiomatic Swift Result

The C function is now exposed in Swift as a read-only computed property named `queue`. This results in a cleaner, more intuitive syntax for accessing the value.

```swift
// Resulting Swift Interface
extension WGPUDevice {
    public var queue: WGPUQueue { get }
}

// Example Usage
let queue = myDevice.queue
```

### Achieving the Same with API Notes

To achieve this transformation without editing the header, add the following entry to your `WebGPU.apinotes` file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Functions:
  - Name: wgpuDeviceGetQueue
    SwiftName: "getter:WGPUDevice.queue(self:)"
```