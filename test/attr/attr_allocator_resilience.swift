// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name FragileBases -emit-module-path %t/FragileBases.swiftmodule %t/FragileBases.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -module-name main -package-name Package -I %t %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution -module-name main -package-name Package -I %t %t/main.swift -default-class-allocator main.Pool -verify-additional-prefix default-

// REQUIRES: swift_feature_ClassAllocators

// In a module built with library evolution, only classes that clients
// outside its package can't see, and that aren't declared `open`, may have
// allocators: clients can inline the allocating initializers of public and
// @usableFromInline classes, and other modules can subclass open ones.
// Package classes may have them (see attr_allocator_package.swift).

//--- FragileBases.swift

public enum FragilePool: @unsafe ClassAllocator {
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

@allocator(FragilePool)
open class FragileBase {
  public init() {}
}

//--- main.swift

import FragileBases // expected-warning {{module 'FragileBases' was not compiled with library evolution support}}

enum Pool: @unsafe ClassAllocator {
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

// Classes that clients can't see may have allocators, declared, inherited or
// from the default, and the allocator type may be internal.
@allocator(Pool)
final class InternalLeaf {}

@allocator(Pool)
class InternalBase {}

final class InternalSub: InternalBase {}

class InternalDefaulted {}

private final class PrivateDefaulted {}

final class InternalSubOfFragile: FragileBase {}

struct Namespace {
  @allocator(Pool)
  public final class NestedInInternal {}

  // But not a class declared `open`, whatever its effective visibility.
  @allocator(Pool)
  open class OpenNestedInInternal {} // expected-error@-1 {{'@allocator' cannot be applied to class 'OpenNestedInInternal' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}

  @allocator(SystemClassAllocator)
  open class OpenOptedOutNestedInInternal {}
}

public struct PublicNamespace {
  @allocator(Pool)
  final class InternalNestedInPublic {}
}

@allocator(Pool)
actor InternalActor {}

actor InternalDefaultedActor {}

// Classes that clients can see can't.
@allocator(Pool)
public final class PublicLeaf {} // expected-error@-1 {{'@allocator' cannot be applied to class 'PublicLeaf' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}

@allocator(Pool)
open class OpenBase {} // expected-error@-1 {{'@allocator' cannot be applied to class 'OpenBase' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}

@allocator(Pool)
package final class PackageLeaf {} // expected-error@-1 {{allocator type 'Pool' of package class 'PackageLeaf' must be '@usableFromInline', package or public because 'Pool' is internal}}

@allocator(Pool)
@usableFromInline
final class UsableFromInlineLeaf {} // expected-error@-2 {{'@allocator' cannot be applied to class 'UsableFromInlineLeaf' because it is '@usableFromInline' in a module built with library evolution, so clients outside its package can see it}}

@allocator(Pool)
@_fixed_layout
public class FixedLayout {} // expected-error@-2 {{'@allocator' cannot be applied to class 'FixedLayout' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}

@allocator(Pool)
public actor PublicActor {} // expected-error@-1 {{'@allocator' cannot be applied to actor 'PublicActor' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}

// The error is on the class that declares the allocator, not its subclasses.
@allocator(Pool)
public class PublicBase {} // expected-error@-1 {{'@allocator' cannot be applied to class 'PublicBase' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}

public final class PublicSub: PublicBase {}

// An allocator inherited from another module's class.
public final class PublicSubOfFragile: FragileBase {} // expected-error {{inherited allocator 'FragilePool' cannot be applied to class 'PublicSubOfFragile' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
// expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

open class OpenSubOfFragile: FragileBase {} // expected-error {{inherited allocator 'FragilePool' cannot be applied to class 'OpenSubOfFragile' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
// expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

package final class PackageSubOfFragile: FragileBase {}

// Also when the open class is nested in a package or internal type.
package enum PackageNamespace {
  open class OpenInPackageSubOfFragile: FragileBase {} // expected-error {{inherited allocator 'FragilePool' cannot be applied to class 'OpenInPackageSubOfFragile' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
  // expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}

  @allocator(SystemClassAllocator)
  open class OpenInPackageOptedOutOfFragile: FragileBase {}

  public final class PublicInPackageSubOfFragile: FragileBase {}
}

enum InternalNamespace {
  open class OpenInInternalSubOfFragile: FragileBase {} // expected-error {{inherited allocator 'FragilePool' cannot be applied to class 'OpenInInternalSubOfFragile' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
  // expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}
}

@usableFromInline
final class UsableFromInlineSubOfFragile: FragileBase {} // expected-error {{inherited allocator 'FragilePool' cannot be applied to class 'UsableFromInlineSubOfFragile' because it is '@usableFromInline' in a module built with library evolution, so clients outside its package can see it}}
// expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{-1:1-1=@allocator(SystemClassAllocator) }}

// Opting out is always allowed.
@allocator(SystemClassAllocator)
public final class PublicOptedOutOfFragile: FragileBase {}

@allocator(SystemClassAllocator)
open class OpenOptedOut {}

@allocator(SystemClassAllocator)
package class PackageOptedOut {}

@allocator(SystemClassAllocator)
@usableFromInline
class UsableFromInlineOptedOut {}

// Under -default-class-allocator, classes that clients can see must opt out.
public class DefaultedPublic {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedPublic' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

open class DefaultedOpen {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedOpen' because it is declared 'open' in a module built with library evolution, so subclasses in other modules could inherit the allocator}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

package class DefaultedPackage {} // expected-default-error {{default class allocator 'Pool' of package class 'DefaultedPackage' must be '@usableFromInline', package or public because 'Pool' is internal}}

@usableFromInline
class DefaultedUsableFromInline {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedUsableFromInline' because it is '@usableFromInline' in a module built with library evolution, so clients outside its package can see it}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{-1:1-1=@allocator(SystemClassAllocator) }}

public actor DefaultedPublicActor {} // expected-default-error {{default class allocator 'Pool' cannot be applied to actor 'DefaultedPublicActor' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

@_fixed_layout
public class DefaultedFixedLayout {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedFixedLayout' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
// expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{-1:1-1=@allocator(SystemClassAllocator) }}

public struct DefaultedNamespace {
  public class DefaultedNested {} // expected-default-error {{default class allocator 'Pool' cannot be applied to class 'DefaultedNested' because it is 'public' in a module built with library evolution, so clients outside its package can see it}}
  // expected-default-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{3-3=@allocator(SystemClassAllocator) }}
}
