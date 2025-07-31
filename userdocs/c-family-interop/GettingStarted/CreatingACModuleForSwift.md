# Creating a C Module for Swift

To use a C library in Swift, you first need to make its declarations visible to the Swift compiler. You do this by creating a *module map*, a special file that groups one or more C header files into a single, importable unit called a module.

## Overview

A module map acts as a wrapper around existing C headers. It tells the Swift compiler which headers to parse and how to combine them into a logical module. This approach allows you to organize C code for Swift consumption without modifying the original source files.

The module map file is typically named `module.modulemap` and resides alongside the C headers it describes.

## The WebGPU Header

For our case study, assume you have the following C header file, `webgpu.h`. This file declares the public API for the WebGPU library.

```c
// webgpu.h

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// An opaque handle to a WebGPU instance.
typedef struct WGPUInstanceImpl* WGPUInstance;

// An opaque handle to a WebGPU device.
typedef struct WGPUDeviceImpl* WGPUDevice;

// An opaque handle to a WebGPU buffer.
typedef struct WGPUBufferImpl* WGPUBuffer;

// An opaque handle to a WebGPU queue.
typedef struct WGPUQueueImpl* WGPUQueue;

// -- Lifecycle Functions --

// Creates a WebGPU instance.
// Returns NULL if the instance cannot be created.
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);

// Releases the instance and its resources.
void wgpuInstanceRelease(WGPUInstance instance);

// Releases the device and its resources.
void wgpuDeviceRelease(WGPUDevice device);

// Releases the buffer and its resources.
void wgpuBufferRelease(WGPUBuffer buffer);

// -- Instance Functions --

// Gets the queue from a device.
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device);

// Writes data to a buffer.
void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer,
                          uint64_t bufferOffset, const void* data, size_t size);

// -- Enumerations and Constants --

typedef enum {
    WGPUAdapterType_DiscreteGPU = 0x00000001,
    WGPUAdapterType_IntegratedGPU = 0x00000002,
    WGPUAdapterType_CPU = 0x00000003,
    WGPUAdapterType_Unknown = 0x00000004
} WGPUAdapterType;

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

// Descriptor structures
typedef struct WGPUInstanceDescriptor {
    const char* nextInChain;
} WGPUInstanceDescriptor;
```

## Create a Module Map File

Next, create a new file named `module.modulemap` in the same directory as `webgpu.h`. This file defines a module named `WebGPU` and specifies that `webgpu.h` contains the module's public interface.

```c
// module.modulemap

module WebGPU {
    header "webgpu.h"
    export *
}
```

- **`module WebGPU`**: Defines a new module with the name `WebGPU`. Your Swift code will use this name to import the module.
- **`header "webgpu.h"`**: Specifies that `webgpu.h` is a public header in this module. Its declarations will be visible to Swift.
- **`export *`**: Makes all symbols imported from the headers in this module available to code that imports this module.

## Organize Your Project Files

//FIXME: <explaination of issue>
// The documentation is currently Swift Package Manager specific but should show direct swiftc usage

For the compiler to find your new module, you need to tell it where to look. Create a directory (for example, `WebGPU`) and place both `webgpu.h` and `module.modulemap` inside it.

```
YourProject/
├── main.swift
└── WebGPU/
    ├── webgpu.h
    └── module.modulemap
```

Then, configure your build system to include the `WebGPU` directory in its import paths. When using the Swift compiler from the command line, you use the `-I` flag:

```sh
swiftc -I./WebGPU main.swift
```

With this structure in place, the `WebGPU` module is now ready to be imported and used in your Swift code.
