# Transforming Factory Functions into Initializers

In C, it is common to use a "factory" function (often with a "Create" prefix) to allocate and return a new instance of an object. Swift encapsulates this concept in its initializers (`init`). By mapping a C factory function to a Swift initializer, you enable natural, idiomatic object creation.

You can use `swift_name` to import a C function that returns a new object as a native Swift initializer.

### The C Starting Point

The `createInstance` function in `example.h` allocates and returns a new `Instance`. It returns `NULL` if creation fails.

<!-- test-block: c-header -->
```c
typedef struct InstanceDescriptor {
    int dummy;
} InstanceDescriptor;

typedef struct Instance *Instance;

Instance createInstance(const InstanceDescriptor* descriptor);
```

### Initial Swift Import

By default, this is imported as a global function. You create a new instance by calling this function directly.

<!-- test-block: swift-interface-default -->
```swift
public struct InstanceDescriptor {

    public init()

    public init(dummy: Int32)

    public var dummy: Int32
}

public typealias Insance = OpaquePointer

public func createInstance(_ descriptor: UnsafePointer<InstanceDescriptor>!) -> Instance!
```

### Refinement with C Annotations

You use the `swift_name` attribute to redefine the factory function as an initializer for the `Instance` type. The function name `init` is a special keyword that tells the compiler to import the C function as an initializer.

<!-- test-block: c-header-annotated -->
```c
typedef struct InstanceDescriptor {
  int dummy;
} InstanceDescriptor;

typedef struct Instance *Instance;

Instance createInstance(const InstanceDescriptor* descriptor)
  __attribute__((swift_name("Instance.init(descriptor:)")));
```

This attribute maps:
- The function to an `init` on the `Instance` type.
- The C parameter `descriptor` to a Swift parameter with the label `descriptor`.

Because the original C function is nullable (it can return `NULL`), Swift imports this as a failable initializer (`init!`).

### Idiomatic Swift Result

The C function is now exposed as a native initializer in Swift. This allows you to create instances using standard Swift syntax, which is more intuitive and consistent with other Swift APIs.

<!-- test-block: swift-interface-refined -->
```swift
public struct InstanceDescriptor {

    public init()

    public init(dummy: Int32)

    public var dummy: Int32
}

public typealias Instance = OpaquePointer
```

### Achieving the Same with API Notes

To apply this refinement without modifying the C header, add the following to your `example.apinotes` file.

<!-- test-block: apinotes -->
```yaml
Name: Example
Functions:
  - Name: createInstance
    SwiftName: "Instance.init(descriptor:)"
```
