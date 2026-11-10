// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Allocators -emit-module-path %t/Allocators.swiftmodule %S/Inputs/class_allocators.swift

// Valid uses, including nested types and the module being compiled.
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.Pool
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.Namespace.NestedPool
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Main -default-class-allocator Main.LocalPool
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -default-class-allocator Swift.SystemClassAllocator

// RUN: not %target-swift-frontend -typecheck %s -target %target-future-triple -I %t -default-class-allocator Allocators.Pool 2>&1 | %FileCheck %s --check-prefix=FEATURE
// FEATURE: error: '-default-class-allocator' requires '-enable-experimental-feature ClassAllocators'

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Pool 2>&1 | %FileCheck %s --check-prefix=NAME
// NAME: error: 'Pool' passed to '-default-class-allocator' is not a fully qualified type name of the form 'Module.Type'

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator 'Allocators.Pool<Int>' 2>&1 | %FileCheck %s --check-prefix=GENERIC-ARGS
// GENERIC-ARGS: error: 'Allocators.Pool<Int>' passed to '-default-class-allocator' is not a fully qualified type name of the form 'Module.Type'

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.Missing 2>&1 | %FileCheck %s --check-prefix=MISSING
// MISSING: error: cannot find type 'Allocators.Missing' for '-default-class-allocator'

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.NotAnAllocator 2>&1 | %FileCheck %s --check-prefix=NONCONFORMING
// NONCONFORMING: error: type 'NotAnAllocator' for '-default-class-allocator' does not conform to protocol 'ClassAllocator'

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.GenericPool 2>&1 | %FileCheck %s --check-prefix=GENERIC
// GENERIC: error: type 'GenericPool<T>' for '-default-class-allocator' must not be generic

// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -module-name Main -default-class-allocator Main.NotAnAllocator 2>&1 | %FileCheck %s --check-prefix=LOCAL-NONCONFORMING
// LOCAL-NONCONFORMING: error: type 'NotAnAllocator' for '-default-class-allocator' does not conform to protocol 'ClassAllocator'

// The runtime support is new, so the deployment target must have it.
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-cpu-apple-macosx10.15 -module-name Main -default-class-allocator Main.LocalPool 2>&1 | %FileCheck %s --check-prefix=DEFAULT-UNAVAILABLE
// DEFAULT-UNAVAILABLE: error: '-default-class-allocator' requires a deployment target of macOS {{[0-9.]+}} or newer

// Weak reference side tables always come from the runtime's allocator; there
// is no flag to choose another one.
// RUN: not %target-swift-frontend -typecheck %s -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -weak-reference-allocator Allocators.Pool 2>&1 | %FileCheck %s --check-prefix=NO-WEAK-FLAG
// NO-WEAK-FLAG: error: unknown argument: '-weak-reference-allocator'

// REQUIRES: swift_feature_ClassAllocators
// REQUIRES: OS=macosx

enum LocalPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

struct NotAnAllocator {}

class C {}
