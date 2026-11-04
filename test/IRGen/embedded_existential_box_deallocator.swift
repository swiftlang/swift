// RUN: %target-swift-emit-ir %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -target arm64-apple-macos14 -wmo -module-name main | %FileCheck %s --check-prefixes=CHECK,ALLOC
// RUN: %target-swift-emit-ir %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -enable-experimental-feature TypedAllocation -target arm64-apple-macos14 -wmo -module-name main | %FileCheck %s --check-prefixes=CHECK,TYPED
// RUN: %target-swift-emit-ir %s -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature NoncopyableCasting -no-allocations -target arm64-apple-macos14 -wmo -module-name main | %FileCheck %s --check-prefixes=CHECK,NOALLOC

// REQUIRES: OS=macosx
// REQUIRES: SWIFT_STDLIB_ARCH=arm64
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation
// REQUIRES: swift_feature_NoncopyableCasting

// The Embedded casting runtime frees the box of an opaque existential after
// taking a non-`Copyable` payload out of it, by calling
// __swift_deallocate_boxed_opaque_existential_0. Only compiled code knows how
// the box was allocated, so every Embedded module defines that helper: it
// deallocates as this module allocates (typed with TypedAllocation), and with
// -no-allocations, where no box can exist, it traps and refers to no
// deallocation function at all.

public protocol P: ~Copyable { }
public struct Small: P, ~Copyable { public var tag = 5 }

@inline(never)
public func take(_ e: consuming any P & ~Copyable) -> Int {
  if let s = e as? Small { return s.tag }
  _ = consume e
  return -1
}

// The runtime's cast logic calls the helper.
// CHECK:       define {{.*}} @"$es7tryCast{{.*}}"(
// CHECK:         call void @__swift_deallocate_boxed_opaque_existential_0(
// CHECK:       {{^}}}

// CHECK-LABEL: define linkonce_odr hidden void @__swift_deallocate_boxed_opaque_existential_0(
// ALLOC:         call void @swift_slowDealloc(
// TYPED:         call void @swift_deallocRawTyped(
// NOALLOC:       call void @llvm.trap()
// NOALLOC-NOT:   call {{.*}}Dealloc
// CHECK:       {{^}}}

// NOALLOC-NOT: @swift_slowDealloc
// NOALLOC-NOT: @swift_deallocRaw
// NOALLOC-NOT: @_swift_deallocate
