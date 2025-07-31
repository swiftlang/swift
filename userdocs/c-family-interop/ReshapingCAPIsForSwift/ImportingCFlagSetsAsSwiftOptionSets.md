# Importing C Flag Sets as Swift Option Sets

A common C pattern is to use integer constants as bit-field flags, which can be combined using bitwise operators. Swift represents this concept with the `OptionSet` protocol, which provides a type-safe, set-like interface.

You can use the `flag_enum` attribute to import a C enumeration of bitmask constants as a Swift `OptionSet`.

### The C Starting Point

The `WGPUBufferUsage` enum in `webgpu.h` defines a set of buffer usage flags using an enumeration where each case is a power of two.

```c
// In webgpu.h
typedef enum {
    WGPUBufferUsage_MapRead = 0x00000001,
    WGPUBufferUsage_MapWrite = 0x00000002,
    WGPUBufferUsage_CopySrc = 0x00000004,
    WGPUBufferUsage_CopyDst = 0x00000008,
    WGPUBufferUsage_Index = 0x00000010,
    WGPUBufferUsage_Vertex = 0x00000020,
    WGPUBufferUsage_Uniform = 0x00000040,
    WGPUBufferUsage_Storage = 0x00000080
} WGPUBufferUsage;
```

### Initial Swift Import

Without the `flag_enum` attribute, Swift would import this as a standard `enum`, with a struct wrapper and global constants for cases. This would not allow for idiomatic set-like operations.

### Refinement with C Annotations

You apply the `flag_enum` attribute to the `enum` definition in the C header. This signals to Swift that the members of this enumeration are intended to be used as bit-field flags.

```c
// In webgpu.h
typedef enum __attribute__((flag_enum)) {
    WGPUBufferUsage_MapRead = 0x00000001,
    WGPUBufferUsage_MapWrite = 0x00000002,
    WGPUBufferUsage_CopySrc = 0x00000004,
    WGPUBufferUsage_CopyDst = 0x00000008,
    WGPUBufferUsage_Index = 0x00000010,
    WGPUBufferUsage_Vertex = 0x00000020,
    WGPUBufferUsage_Uniform = 0x00000040,
    WGPUBufferUsage_Storage = 0x00000080
} WGPUBufferUsage;
```

Like with regular enums, Swift performs prefix stripping on the member names. It also recognizes that this is an option set and imports it as a struct conforming to the `OptionSet` protocol.

### Idiomatic Swift Result

The C flags are now exposed in Swift as a type-safe `OptionSet`. This allows you to work with them using familiar set syntax, like creating an option set from an array literal and checking for membership with the `contains()` method.

```swift
// Resulting Swift Interface
public struct WGPUBufferUsage : OptionSet {
    public init(rawValue: UInt32)
    public var rawValue: UInt32

    public static var mapRead: WGPUBufferUsage { get }
    public static var mapWrite: WGPUBufferUsage { get }
    public static var copySrc: WGPUBufferUsage { get }
    public static var copyDst: WGPUBufferUsage { get }
    public static var index: WGPUBufferUsage { get }
    public static var vertex: WGPUBufferUsage { get }
    public static var uniform: WGPUBufferUsage { get }
    public static var storage: WGPUBufferUsage { get }
}

// Example Usage
let usage: WGPUBufferUsage = [.vertex, .uniform]
if usage.contains(.vertex) {
    print("Buffer can be used for vertex data.")
}
```

### Achieving the Same with API Notes

To get the same `OptionSet` behavior using API Notes, you specify `OptionSet: true` for the type in your `WebGPU.apinotes` file.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Tags:
  - Name: WGPUBufferUsage
    OptionSet: true
```
