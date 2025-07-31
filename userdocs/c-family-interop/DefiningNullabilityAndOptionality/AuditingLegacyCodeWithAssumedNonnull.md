# Auditing Legacy Code with Assumed Non-null

When working with older C libraries that lack any nullability annotations, adding `_Nonnull` to every pointer can be a daunting task. A more efficient strategy is to reverse the assumption: tell the compiler to treat all unannotated pointers as non-null by default.

This approach lets you focus on finding and annotating the small number of pointers that are genuinely nullable, dramatically speeding up the process of modernizing a legacy C API.

### The Problem: A Sea of Unsafe Optionals

When an entire header lacks nullability annotations, its Swift interface becomes littered with implicitly unwrapped optionals (`!`). This makes the API unsafe and forces you to defensively check for `nil` or risk runtime crashes.

```c
// Original header with no annotations
typedef struct WGPUInstanceImpl* WGPUInstance;
typedef struct WGPUDeviceImpl* WGPUDevice;
WGPUInstance wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);
void wgpuInstanceRelease(WGPUInstance instance);
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device);
```

```swift
// Resulting Swift Interface
public init!(descriptor: UnsafePointer<WGPUInstanceDescriptor>!) -> WGPUInstance!
public func wgpuInstanceRelease(_ instance: WGPUInstance!)
public var queue: WGPUQueue! { get }
```

### The Solution: Assume Non-null and Audit for Exceptions

You can wrap your C header's content in the `NS_ASSUME_NONNULL_BEGIN` and `NS_ASSUME_NONNULL_END` macros. Between these two macros, the compiler will assume every simple pointer type is non-null unless it is explicitly marked `_Nullable`.

This flips your workflow:
1.  **Wrap the entire header** in the `NS_ASSUME_NONNULL_...` macros.
2.  **Re-examine the Swift interface.** Most of the `!` will have disappeared.
3.  **Audit the C API.** Your new task is to find the few cases where a `NULL` is possible and explicitly annotate them with `_Nullable`.

### Applying the Assumed Non-null Strategy

Here is how you would apply this technique to `webgpu.h`.

```c
// In webgpu.h

#if __cplusplus
extern "C" {
#endif

// Tell the compiler to assume non-null from this point forward.
NS_ASSUME_NONNULL_BEGIN

typedef struct WGPUInstanceImpl* WGPUInstance;
typedef struct WGPUDeviceImpl* WGPUDevice;

// Audit result: This function is the only one that can return NULL. Mark it explicitly.
WGPUInstance _Nullable wgpuCreateInstance(const WGPUInstanceDescriptor* descriptor);

// All other pointers are now implicitly non-null.
void wgpuInstanceRelease(WGPUInstance instance);
WGPUQueue wgpuDeviceGetQueue(WGPUDevice device);

// Stop assuming non-null.
NS_ASSUME_NONNULL_END

#if __cplusplus
}
#endif
```

### The Modernized Swift Result

The resulting Swift interface is now clean, safe, and accurately reflects the API contract. The implicitly unwrapped optionals are gone, replaced by non-optional types and a single, correctly-typed optional where `nil` is a possible value.

```swift
// Resulting Swift Interface
public init!(descriptor: UnsafePointer<WGPUInstanceDescriptor>) -> WGPUInstance? // Correctly optional
public func wgpuInstanceRelease(_ instance: WGPUInstance) // Correctly non-optional
public var queue: WGPUQueue { get } // Correctly non-optional
```

This bulk-editing approach provides a pragmatic and highly effective path for bringing the safety and clarity of Swift's optional system to large, unannotated C codebases.
