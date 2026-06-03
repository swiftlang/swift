// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift(-import-objc-header %S/Inputs/refcount_hooks.h)

// REQUIRES: executable_test
// REQUIRES: OS=macosx || OS=linux-gnu

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Override _swift_allocObject with a no-op wrapper that forwards to the
// original implementation. This ensures the runtime goes through the hooks
// path and exercises the hooks machinery without crashing.

var originalAllocObject: AllocObjectFn = _swift_allocObject

func hookedAllocObject(
  _ metadata: UnsafeRawPointer?,
  _ requiredSize: Int,
  _ requiredAlignmentMask: Int
) -> UnsafeMutableRawPointer? {
  return originalAllocObject(metadata, requiredSize, requiredAlignmentMask)
}

_swift_allocObject = hookedAllocObject

// A single print does plenty of refcounting to exercise the hooks.
print("Hello, world.")
