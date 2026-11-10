// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -swift-version 5 -enable-library-evolution -enable-experimental-feature ClassAllocators -target %target-future-triple %s -module-name Library -package-name Package -emit-module-path %t/Library.swiftmodule -emit-module-interface-path %t/Library.swiftinterface -emit-private-module-interface-path %t/Library.private.swiftinterface -emit-package-module-interface-path %t/Library.package.swiftinterface -default-class-allocator Library.DefaultPool
// RUN: %FileCheck %s < %t/Library.package.swiftinterface
// RUN: %FileCheck %s --check-prefix PUBLIC < %t/Library.swiftinterface
// RUN: %FileCheck %s --check-prefix PUBLIC < %t/Library.private.swiftinterface

// A module built from the package interface has the allocators of its
// package classes, including those from -default-class-allocator, which the
// interfaces don't record.
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.package.swiftinterface) -module-name Library -target %target-future-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library -target %target-future-triple
// RUN: %empty-directory(%t/rebuilt)
// RUN: %target-swift-frontend -compile-module-from-interface %t/Library.package.swiftinterface -module-name Library -target %target-future-triple -o %t/rebuilt/Library.swiftmodule

// Other modules of the package that import the rebuilt module, or the
// module built from source, know which classes have allocators.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -I %t/rebuilt -D CLIENT %s -module-name Client
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -I %t/rebuilt -D CLIENT %s -module-name Client -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -I %t -D CLIENT %s -module-name Client

// REQUIRES: swift_feature_ClassAllocators

#if !CLIENT
// A package interface prints the package classes' allocators, declared or
// from -default-class-allocator, so their types are package or public. The
// public and private interfaces print neither, only opt-outs.

// CHECK: package enum PackagePool : @unsafe Swift{{(::|\.)}}ClassAllocator
// PUBLIC-NOT: PackagePool
// PUBLIC-NOT: DefaultPool
// PUBLIC-NOT: PackageLeaf
// PUBLIC-NOT: Namespace
package enum PackagePool: @unsafe ClassAllocator {
  package static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  package static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// CHECK: package enum DefaultPool : @unsafe Swift{{(::|\.)}}ClassAllocator
package enum DefaultPool: @unsafe ClassAllocator {
  package static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  package static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

// A declared allocator.
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{(::|\.)}}PackagePool) final package class PackageLeaf {
@allocator(PackagePool)
package final class PackageLeaf {
  package init() {}
}

// The default.
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{(::|\.)}}DefaultPool) final package class DefaultedLeaf {
package final class DefaultedLeaf {
  package init() {}
}

// An inherited allocator is printed on the superclass that declares it.
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{(::|\.)}}PackagePool) package class ExplicitBase {
// CHECK: {{^}}@_inheritsConvenienceInitializers final package class ExplicitSub : Library{{(::|\.)}}ExplicitBase {
@allocator(PackagePool)
package class ExplicitBase {
  package init() {}
}

package final class ExplicitSub: ExplicitBase {}

// A subclass of a class with the default gets the default too.
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{(::|\.)}}DefaultPool) package class DefaultedBase {
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @_inheritsConvenienceInitializers @allocator(Library{{(::|\.)}}DefaultPool) final package class DefaultedSub : Library{{(::|\.)}}DefaultedBase {
package class DefaultedBase {
  package init() {}
}

package final class DefaultedSub: DefaultedBase {}

// An opt-out, and a subclass that inherits it.
// CHECK: @allocator(Swift{{(::|\.)}}SystemClassAllocator) package class OptedOut {
// CHECK: {{^}}@_inheritsConvenienceInitializers final package class OptedOutSub : Library{{(::|\.)}}OptedOut {
@allocator(SystemClassAllocator)
package class OptedOut {
  package init() {}
}

package final class OptedOutSub: OptedOut {}

// An `open` class can't have an allocator, so it opts out of the default. A
// public class nested in a package type gets the default.
// CHECK: package enum Namespace
// CHECK: @allocator(Swift{{(::|\.)}}SystemClassAllocator) open class Base {
// CHECK: #if compiler(>=5.3) && $ClassAllocators
// CHECK-NEXT: @allocator(Library{{(::|\.)}}DefaultPool) final public class PublicInPackage {
package enum Namespace {
  @allocator(SystemClassAllocator)
  open class Base {
    package init() {}
  }

  public final class PublicInPackage {
    package init() {}
  }
}

// Internal classes aren't printed.
// CHECK-NOT: InternalDefaulted
final class InternalDefaulted {}

// PUBLIC: @allocator(Swift{{(::|\.)}}SystemClassAllocator) public class PublicClass
// CHECK: @allocator(Swift{{(::|\.)}}SystemClassAllocator) public class PublicClass
@allocator(SystemClassAllocator)
public class PublicClass {}
#else
import Library

final class Sub: Namespace.Base {}

package final class PackageSub: Namespace.Base {}

func makeBuffers() {
  _ = ManagedBufferPointer<Int, Int>(bufferClass: PackageLeaf.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'PackageLeaf', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: DefaultedLeaf.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'DefaultedLeaf', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: ExplicitSub.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'ExplicitSub', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: DefaultedSub.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'DefaultedSub', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Namespace.PublicInPackage.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'Namespace.PublicInPackage', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: OptedOutSub.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Sub.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: PackageSub.self, minimumCapacity: 1) { _, _ in 0 }
}
#endif
