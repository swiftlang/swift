# Inspecting the Default API

After you create a module map, your C library becomes visible to Swift. You can now import the module and see how the Swift compiler translates the C declarations into an initial Swift interface.

## Overview

Swift performs a direct, literal translation of the C headers to generate a default API. This interface is immediately usable but often lacks the safety and clarity of idiomatic Swift code.

Inspecting this default API is a crucial step. It helps you understand the starting point from which you will make your refinements and appreciate the improvements you make along the way.

## Import the C Module

To use the C library, you import the module by the name you defined in your `module.modulemap` file. Add the following line to the top of your Swift file:

```swift
import WebGPU
```

## View the Generated Swift Interface

//FIXME: <explaination of issue>
// According to "Adapting a C header for Swift.md", there is a command-line way using swift-ide-test
// that should be documented here for developers without Xcode

You can view the Swift interface for your C module in several ways:

**In Xcode:** Command-click the module name in your `import` statement and choose "Jump to Definition". This reveals a generated header that shows how Swift sees every type and function from your C library.

**From the command line:** Use the `swift-ide-test` tool to print the interface:

```sh
echo "import WebGPU" > UseWebGPU.swift
swift-ide-test -print-module -module-to-print WebGPU -print-interface -source-filename UseWebGPU.swift -I ./WebGPU
```

The following is the default Swift interface generated for the `WebGPU` module.

```swift
// Default Swift interface for WebGPU

// The C opaque pointers are imported as type aliases for OpaquePointer.
public typealias WGPUInstance = OpaquePointer?
public typealias WGPUDevice = OpaquePointer?
public typealias WGPUBuffer = OpaquePointer?
public typealias WGPUQueue = OpaquePointer?

// C functions are imported as global Swift functions.
// Note the unlabeled parameters and unsafe pointer types.
public func wgpuCreateInstance(_ descriptor: UnsafePointer<WGPUInstanceDescriptor>!) -> WGPUInstance
public func wgpuInstanceRelease(_ instance: WGPUInstance!)
public func wgpuDeviceRelease(_ device: WGPUDevice!)
public func wgpuBufferRelease(_ buffer: WGPUBuffer!)
public func wgpuDeviceGetQueue(_ device: WGPUDevice!) -> WGPUQueue
public func wgpuQueueWriteBuffer(_ queue: WGPUQueue!, _ buffer: WGPUBuffer!, _ bufferOffset: UInt64, _ data: UnsafeRawPointer!, _ size: Int)

// C enums are imported as structs that conform to RawRepresentable.
public struct WGPUAdapterType : Equatable, RawRepresentable {
    public init(rawValue: UInt32)
    public var rawValue: UInt32
}
public var WGPUAdapterType_DiscreteGPU: WGPUAdapterType { get }
public var WGPUAdapterType_IntegratedGPU: WGPUAdapterType { get }
public var WGPUAdapterType_CPU: WGPUAdapterType { get }
public var WGPUAdapterType_Unknown: WGPUAdapterType { get }

// C flag enums are imported as structs that conform to RawRepresentable.
public struct WGPUBufferUsage : Equatable, RawRepresentable {
    public init(rawValue: UInt32)
    public var rawValue: UInt32
}
public var WGPUBufferUsage_MapRead: WGPUBufferUsage { get }
public var WGPUBufferUsage_MapWrite: WGPUBufferUsage { get }
public var WGPUBufferUsage_CopySrc: WGPUBufferUsage { get }
public var WGPUBufferUsage_CopyDst: WGPUBufferUsage { get }
public var WGPUBufferUsage_Index: WGPUBufferUsage { get }
public var WGPUBufferUsage_Vertex: WGPUBufferUsage { get }
public var WGPUBufferUsage_Uniform: WGPUBufferUsage { get }
public var WGPUBufferUsage_Storage: WGPUBufferUsage { get }
```

## Analyze the Default API

This initial translation works, but it exposes several issues that are common when importing C APIs:

- **Unsafe Pointers:** The WebGPU handles are `OpaquePointer` types, which offer no type safety. Buffer parameters use `UnsafeRawPointer`, requiring you to manage memory manually.
- **Global Functions:** The API is a collection of global functions rather than an object-oriented interface with methods. You must pass the handle object to every function.
- **Unclear Call Sites:** Function parameters are unlabeled (`_`). A call to `wgpuQueueWriteBuffer` looks like `wgpuQueueWriteBuffer(queue, buffer, 0, data, size)`, which obscures the meaning of each argument.
- **Clumsy Enums:** The `WGPUAdapterType` enumeration is imported as a struct, with its cases (`WGPUAdapterType_DiscreteGPU`) exposed as separate global constants.

This default interface is our baseline. In the following sections, you will learn how to address each of these issues to create a truly Swift-native API.
