// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// In a module built with library evolution, package classes may have
// allocators, with or without Package CMO. Classes that clients outside the
// package can see can't, nor can classes declared `open`, even when they're
// nested in a package or internal type.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Library %t/Library.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Library %t/Library.swift -allow-non-resilient-access
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Library %t/Library.swift -allow-non-resilient-access -package-cmo

// Under -default-class-allocator, the classes that can't have one are errors.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Library %t/Library.swift -default-class-allocator Library.PackagePool -verify-additional-prefix default-
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Library %t/Library.swift -allow-non-resilient-access -package-cmo -default-class-allocator Library.PackagePool -verify-additional-prefix default-

// The allocator of a package class must be visible to the other modules of
// its package, with or without library evolution.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name Visibility %t/Visibility.swift -default-class-allocator Visibility.InternalPool -verify-additional-prefix evolution-
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -module-name Visibility %t/Visibility.swift -default-class-allocator Visibility.InternalPool -verify-additional-prefix fragile-

// Without library evolution, classes declared `open` may have allocators,
// declared, inherited or from -default-class-allocator.
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -module-name Fragile %t/Fragile.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -module-name Fragile %t/Fragile.swift -default-class-allocator Fragile.PublicPool

// Other modules of the package can't subclass a package class, but they can
// subclass an `open` class nested in a package type, which can't have an
// allocator to inherit; their subclasses may declare their own. A client in
// another package can't see them.
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -package-name Package -module-name PackageLibrary %t/PackageLibrary.swift -emit-module-path %t/PackageLibrary.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -I %t %t/Client.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name Package -I %t %t/Client.swift -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -package-name OtherPackage -I %t %t/OtherPackage.swift

// REQUIRES: swift_feature_ClassAllocators

//--- Library.swift

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

public enum PublicPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

enum InternalPool: @unsafe ClassAllocator {
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

// Package classes, declared or inherited, and classes nested in package
// types, whatever their access level.
@allocator(PackagePool)
package final class PackageLeaf {}

@allocator(PublicPool)
package class PackageBase {}

package final class PackageSub: PackageBase {}

@allocator(PackagePool)
package actor PackageActor {}

package enum Namespace {
  @allocator(PackagePool)
  public final class PublicInPackage {}
}

// Internal classes may have allocators, and the allocator type may be
// internal.
@allocator(InternalPool)
final class InternalLeaf {}

// The other modules of the package see a package class's allocator, so it
// must be visible to them.
@allocator(InternalPool)
package final class InternalPoolLeaf {} // expected-error@-1 {{allocator type 'InternalPool' of package class 'InternalPoolLeaf' must be '@usableFromInline', package or public because 'InternalPool' is internal}}

// Classes that clients outside the package can see can't have allocators.
@allocator(PublicPool)
public final class PublicLeaf {} // expected-error@-1 {{'@allocator' cannot be applied to class 'PublicLeaf' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}

@allocator(PublicPool)
open class OpenBase {} // expected-error@-1 {{'@allocator' cannot be applied to class 'OpenBase' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}

@allocator(PublicPool)
@usableFromInline
package final class UsableFromInlinePackage {} // expected-error@-2 {{'@allocator' cannot be applied to class 'UsableFromInlinePackage' because it is '@usableFromInline' in a module built with library evolution, so clients outside its package can see it}}

@allocator(PublicPool)
@usableFromInline
final class UsableFromInlineInternal {} // expected-error@-2 {{'@allocator' cannot be applied to class 'UsableFromInlineInternal' because it is '@usableFromInline' in a module built with library evolution, so clients outside its package can see it}}

// Nor can a class declared `open` that's nested in a package or internal
// type, although clients outside the package can't see it: the other modules
// of the package (or `@testable` importers) could subclass it. (The
// superclass of an open class is open, so the only allocator it could
// inherit is declared on a superclass that can't have one either, and is
// diagnosed there, or comes from a module built without library evolution;
// see attr_allocator_resilience.swift.)
package enum OpenNamespace {
  @allocator(PackagePool)
  open class OpenInPackage {} // expected-error@-1 {{'@allocator' cannot be applied to class 'OpenInPackage' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}

  open class OpenSubInPackage: OpenInPackage {}

  // Opting out is allowed.
  @allocator(SystemClassAllocator)
  open class OpenOptedOutInPackage {}

  // A subclass that isn't open may have one.
  @allocator(PackagePool)
  package final class PackageSubOfOpen: OpenOptedOutInPackage {}
}

enum InternalNamespace {
  @allocator(InternalPool)
  open class OpenInInternal {} // expected-error@-1 {{'@allocator' cannot be applied to class 'OpenInInternal' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}

  @allocator(SystemClassAllocator)
  open class OpenOptedOutInInternal {}

  @allocator(InternalPool)
  final class InternalSubOfOpen: OpenOptedOutInInternal {}
}

// A package subclass of a public class may have an allocator.
@allocator(SystemClassAllocator)
public class PublicOptedOut {}

@allocator(PackagePool)
package final class PackageSubOfPublic: PublicOptedOut {}

@allocator(SystemClassAllocator)
package class PackageOptedOut {}

package final class PackageSubOfOptedOut: PackageOptedOut {}

// Under -default-class-allocator, classes that clients outside the package
// can see, and classes declared `open`, must opt out.
package class DefaultedPackage {}

package enum DefaultedNamespace {
  open class DefaultedOpenInPackage {} // expected-default-error {{default class allocator 'PackagePool' cannot be applied to class 'DefaultedOpenInPackage' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
  // expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}

  @allocator(SystemClassAllocator)
  open class DefaultedOpenOptedOutInPackage {}

  public final class DefaultedPublicInPackage {}
}

enum DefaultedInternalNamespace {
  open class DefaultedOpenInInternal {} // expected-default-error {{default class allocator 'PackagePool' cannot be applied to class 'DefaultedOpenInInternal' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
  // expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}
}

public class DefaultedPublic {} // expected-default-error {{default class allocator 'PackagePool' cannot be applied to class 'DefaultedPublic' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

class DefaultedInternal {}

//--- Visibility.swift

enum InternalPool: @unsafe ClassAllocator {
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

@usableFromInline
enum UsableFromInlinePool: @unsafe ClassAllocator {
  @usableFromInline
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  @usableFromInline
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

package class DefaultedPackage {} // expected-error {{default class allocator 'InternalPool' of package class 'DefaultedPackage' must be '@usableFromInline', package or public because 'InternalPool' is internal}}

package enum Namespace {
  open class DefaultedOpenInPackage {} // expected-fragile-error {{default class allocator 'InternalPool' of open class 'DefaultedOpenInPackage' must be '@usableFromInline', package or public because 'InternalPool' is internal}}
  // expected-evolution-error@-1 {{default class allocator 'InternalPool' cannot be applied to class 'DefaultedOpenInPackage' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
  // expected-evolution-note@-2 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}
}

class DefaultedInternal {}

@allocator(PackagePool)
package final class PackageAllocator {}

@allocator(UsableFromInlinePool)
package final class UsableFromInlineAllocator {}

@allocator(SystemClassAllocator)
package final class OptedOut {}

//--- Fragile.swift

public enum PublicPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    unsafe pointer.deallocate()
  }
}

@allocator(PublicPool)
package class PackageBase {}

@allocator(PublicPool)
open class OpenBase {}

open class OpenSub: OpenBase {}

package enum Namespace {
  @allocator(PublicPool)
  open class OpenInPackage {}

  open class OpenSubInPackage: OpenInPackage {}

  open class DefaultedOpenInPackage {}
}

enum InternalNamespace {
  @allocator(PublicPool)
  open class OpenInInternal {}

  open class DefaultedOpenInInternal {}
}

open class DefaultedOpen {}

//--- PackageLibrary.swift

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

@allocator(PackagePool)
package class PackageBase {
  package init() {}
}

@allocator(PackagePool)
package final class PackageLeaf {
  package init() {}
}

package enum Namespace {
  open class Base {
    package init() {}
  }

  @allocator(SystemClassAllocator)
  open class OptedOut {
    package init() {}
  }
}

//--- Client.swift

import PackageLibrary

// As for any package class.
final class SubOfPackageClass: PackageBase {} // expected-error {{cannot inherit from non-open class 'PackageBase' outside of its defining module}}

// An `open` class nested in a package type has no allocator to inherit, but
// a subclass may declare its own, such as the library's package allocator.
final class Sub: Namespace.Base {}

package final class PackageSub: Namespace.Base {}

@allocator(PackagePool)
final class OwnSub: Namespace.Base {}

final class OptedOutSub: Namespace.OptedOut {}

// The client knows which classes have allocators.
func makeBuffers() {
  _ = ManagedBufferPointer<Int, Int>(bufferClass: PackageLeaf.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'PackageLeaf', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: Sub.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: PackageSub.self, minimumCapacity: 1) { _, _ in 0 }
  _ = ManagedBufferPointer<Int, Int>(bufferClass: OwnSub.self, minimumCapacity: 1) { _, _ in 0 } // expected-error {{'ManagedBufferPointer' cannot allocate an instance of 'OwnSub', which has a class allocator}}
  _ = ManagedBufferPointer<Int, Int>(bufferClass: OptedOutSub.self, minimumCapacity: 1) { _, _ in 0 }
}

//--- OtherPackage.swift

import PackageLibrary

final class Sub: Namespace.Base {} // expected-error {{cannot find type 'Namespace' in scope}}
