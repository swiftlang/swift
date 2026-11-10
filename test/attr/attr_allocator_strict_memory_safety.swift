// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Pools -emit-module-path %t/Pools.swiftmodule %t/Pools.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t %t/main.swift -strict-memory-safety -verify-additional-prefix strict-
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t %t/main.swift -strict-memory-safety:migrate -verify-additional-prefix strict-
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t %t/main.swift

// REQUIRES: swift_feature_ClassAllocators

// Under strict memory safety, a conformance to ClassAllocator must be written
// `@unsafe ClassAllocator`. Without strict memory safety, it's accepted either
// way.

//--- Pools.swift

// Built without strict memory safety, and imported by main.swift, which isn't
// diagnosed for the conformances of the modules it imports.
public enum ImportedPool: ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer.allocate(byteCount: byteCount, alignment: alignment)
  }
  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    pointer.deallocate()
  }
}

@available(SwiftStdlib 6.5, *)
public protocol ImportedRefinement: ClassAllocator {}

//--- main.swift

import Pools

// The conformances of imported modules, including the standard library's
// `SystemClassAllocator: @unsafe ClassAllocator`, aren't diagnosed.
@allocator(ImportedPool)
final class UsesImportedPool {}

@allocator(SystemClassAllocator)
final class UsesSystemClassAllocator {}

// Explicit conformances on the type and in an extension.
enum Plain: ClassAllocator {
  // expected-strict-warning@-1 {{conformance of 'Plain' to protocol 'ClassAllocator' must be marked '@unsafe' because the compiler trusts a class allocator to manage memory correctly}}{{documentation-file=strict-memory-safety}}{{13-13=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum Marked: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

@allocator(Plain)
final class UsesPlain {}

@allocator(Marked)
final class UsesMarked {}

enum InExtension {}

extension InExtension: ClassAllocator {
  // expected-strict-warning@-1 {{conformance of 'InExtension' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{24-24=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum MarkedInExtension {}

extension MarkedInExtension: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

struct GenericPool<T>: ClassAllocator {
  // expected-strict-warning@-1 {{conformance of 'GenericPool<T>' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{24-24=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

// A conformance implied by a conformance to a protocol that refines
// ClassAllocator is acknowledged by writing that conformance `@unsafe`, which
// is where the Fix-It goes. Marking the refining protocol `@unsafe` doesn't
// acknowledge it.
@available(SwiftStdlib 6.5, *)
protocol Refinement: ClassAllocator {}
@available(SwiftStdlib 6.5, *)
@unsafe protocol UnsafeRefinement: ClassAllocator {}

enum ViaRefinement: Refinement {
  // expected-strict-warning@-1 {{conformance of 'ViaRefinement' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{21-21=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum MarkedViaRefinement: @unsafe Refinement {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum ViaUnsafeRefinement: UnsafeRefinement {
  // expected-strict-warning@-1 {{conformance of 'ViaUnsafeRefinement' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{27-27=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum ViaImportedRefinement {}

extension ViaImportedRefinement: ImportedRefinement {
  // expected-strict-warning@-1 {{conformance of 'ViaImportedRefinement' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{34-34=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

// The conformances of an `@unsafe` type are already unsafe, as for unsafe
// witnesses of safe requirements. An `@unsafe` extension of a safe type
// doesn't make its conformances unsafe.
@unsafe enum UnsafeType: ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

@unsafe enum UnsafeTypeWithExtension {}

extension UnsafeTypeWithExtension: ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

enum SafeTypeWithUnsafeExtension {}

@unsafe extension SafeTypeWithUnsafeExtension: ClassAllocator {
  // expected-strict-warning@-1 {{conformance of 'SafeTypeWithUnsafeExtension' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{48-48=@unsafe }}
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

// A subclass's inherited conformance isn't diagnosed again.
class ClassPool: ClassAllocator {
  // expected-strict-warning@-1 {{conformance of 'ClassPool' to protocol 'ClassAllocator' must be marked '@unsafe'}}{{18-18=@unsafe }}
  class func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer { fatalError() }
  class func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {}
}

final class ClassPoolSubclass: ClassPool {}

@allocator(ClassPoolSubclass)
final class UsesClassPoolSubclass {}
