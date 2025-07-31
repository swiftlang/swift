# Importing C Enumerations as Swift Enums

C enumerations are essentially named integer constants. By default, Swift imports them as a structure with a raw value and a set of global constants for the cases. You can provide a much safer and more idiomatic interface by importing a C `enum` as a native Swift `enum`.

The `enum_extensibility` attribute tells Swift to import a C enumeration as a true Swift `enum`, with strongly-typed cases.

### The C Starting Point

The `WGPUAdapterType` enum in `webgpu.h` defines different types of GPU adapters.

```c
// In webgpu.h
typedef enum {
    WGPUAdapterType_DiscreteGPU = 0x00000001,
    WGPUAdapterType_IntegratedGPU = 0x00000002,
    WGPUAdapterType_CPU = 0x00000003,
    WGPUAdapterType_Unknown = 0x00000004
} WGPUAdapterType;
```

### Initial Swift Import

Swift imports this as a `RawRepresentable` struct. The individual cases are imported as global variables of that struct type, forcing you to use the long-form names.

```swift
// Default Swift Interface
public struct WGPUAdapterType : Equatable, RawRepresentable {
    public init(rawValue: UInt32)
    public var rawValue: UInt32
}
public var WGPUAdapterType_DiscreteGPU: WGPUAdapterType { get }
public var WGPUAdapterType_IntegratedGPU: WGPUAdapterType { get }
public var WGPUAdapterType_CPU: WGPUAdapterType { get }
public var WGPUAdapterType_Unknown: WGPUAdapterType { get }

// Example Usage
let adapterType = WGPUAdapterType_DiscreteGPU
```

### Refinement with C Annotations

You add the `enum_extensibility(closed)` attribute to the C `enum` definition. A "closed" enum means that no other cases will ever be added. Swift uses this information to perform more powerful static analysis.

```c
// In webgpu.h
typedef enum __attribute__((enum_extensibility(closed))) {
    WGPUAdapterType_DiscreteGPU = 0x00000001,
    WGPUAdapterType_IntegratedGPU = 0x00000002,
    WGPUAdapterType_CPU = 0x00000003,
    WGPUAdapterType_Unknown = 0x00000004
} WGPUAdapterType;
```
When this attribute is present, Swift automatically performs prefix stripping. It identifies the type name (`WGPUAdapterType`) as a common prefix on the case names (`WGPUAdapterType_DiscreteGPU`) and removes it, resulting in clean case names like `.discreteGPU`.

### Idiomatic Swift Result

The C `enum` is now a native, type-safe Swift `enum`. This allows you to use dot syntax for cases and benefit from Swift's exhaustive `switch` statement checking.

```swift
// Resulting Swift Interface
//FIXME: <explaination of issue>
// According to the old reference documentation (CToSwiftNameTranslation.md),
// enum_extensibility imports as @objc enums, not regular Swift enums
@objc public enum WGPUAdapterType : Int {
    case discreteGPU = 1
    case integratedGPU = 2
    case CPU = 3
    case unknown = 4
}

// Example Usage
let adapterType: WGPUAdapterType = .discreteGPU
switch adapterType {
case .discreteGPU:
    print("Using discrete GPU")
case .integratedGPU:
    print("Using integrated GPU")
case .CPU:
    print("Using CPU")
case .unknown:
    print("Unknown adapter type")
}
```

### Achieving the Same with API Notes

To import the C `enum` as a Swift `enum` using external metadata, add the following to your `WebGPU.apinotes` file. The `Tags` key is used to annotate types like structs and enums.

```yaml
# In WebGPU.apinotes
---
Name: WebGPU
Tags:
  - Name: WGPUAdapterType
    EnumExtensibility: closed
```
