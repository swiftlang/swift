// RUN: %empty-directory(%t)

// A library built without library evolution, and one built with it.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -enable-experimental-feature ClassAllocators -target %target-future-triple %S/Inputs/class_allocator_fragile_library.swift -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library -default-class-allocator Library.LibraryPool -Onone
// RUN: %target-codesign %t/%target-library-name(Library)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ResilientLibrary)) -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution %S/Inputs/class_allocator_resilient_library.swift -emit-module -emit-module-path %t/ResilientLibrary.swiftmodule -module-name ResilientLibrary -O
// RUN: %target-codesign %t/%target-library-name(ResilientLibrary)

// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lLibrary -lResilientLibrary -I %t -L %t -o %t/main-Onone -Onone -Xfrontend -sil-verify-all %target-rpath(%t)
// RUN: %target-codesign %t/main-Onone
// RUN: %target-run %t/main-Onone %t/%target-library-name(Library) %t/%target-library-name(ResilientLibrary)

// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lLibrary -lResilientLibrary -I %t -L %t -o %t/main-O -O -Xfrontend -sil-verify-all %target-rpath(%t)
// RUN: %target-codesign %t/main-O
// RUN: %target-run %t/main-O %t/%target-library-name(Library) %t/%target-library-name(ResilientLibrary)

// IRGen allocates no stack-promoted object on the stack.
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lLibrary -lResilientLibrary -I %t -L %t -o %t/main-O-heap -O -Xfrontend -sil-verify-all -Xfrontend -stack-promotion-limit -Xfrontend 0 -D HEAP_FALLBACK %target-rpath(%t)
// RUN: %target-codesign %t/main-O-heap
// RUN: %target-run %t/main-O-heap %t/%target-library-name(Library) %t/%target-library-name(ResilientLibrary)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Library
import ResilientLibrary
import StdlibUnittest
import Synchronization

struct Counts: Equatable {
  var allocations, deallocations: Int
}

func counting(_ body: () -> Void) -> Counts {
  let allocations = libraryPoolAllocations.load(ordering: .relaxed)
  let deallocations = libraryPoolDeallocations.load(ordering: .relaxed)
  body()
  return Counts(
    allocations: libraryPoolAllocations.load(ordering: .relaxed) - allocations,
    deallocations:
      libraryPoolDeallocations.load(ordering: .relaxed) - deallocations)
}

func countingResilient(_ body: () -> Void) -> Counts {
  let allocations = resilientPoolAllocations.load(ordering: .relaxed)
  let deallocations = resilientPoolDeallocations.load(ordering: .relaxed)
  body()
  return Counts(
    allocations:
      resilientPoolAllocations.load(ordering: .relaxed) - allocations,
    deallocations:
      resilientPoolDeallocations.load(ordering: .relaxed) - deallocations)
}

let clientPoolAllocations = Atomic<Int>(0)
let clientPoolDeallocations = Atomic<Int>(0)

enum ClientPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    clientPoolAllocations.add(1, ordering: .relaxed)
    return UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    clientPoolDeallocations.add(1, ordering: .relaxed)
    unsafe pointer.deallocate()
  }
}

func countingClient(_ body: () -> Void) -> Counts {
  let allocations = clientPoolAllocations.load(ordering: .relaxed)
  let deallocations = clientPoolDeallocations.load(ordering: .relaxed)
  body()
  return Counts(
    allocations: clientPoolAllocations.load(ordering: .relaxed) - allocations,
    deallocations:
      clientPoolDeallocations.load(ordering: .relaxed) - deallocations)
}

// Subclasses of the fragile library's classes inherit their allocators,
// including the one from its -default-class-allocator, and its opt-out.
class ClientDerived: LibraryDerived {
  var y = 2
}

final class ClientLeaf: LibraryBase {}

final class ClientDefaulted: LibraryDefaulted {}

final class ClientOptedOut: LibraryOptedOut {}

// A client subclass may declare its own allocator.
@allocator(ClientPool)
final class ClientOwn: LibraryBase {
  var z = 3
}

// Classes with resilient ancestry, whose metadata the runtime builds.
@allocator(ClientPool)
final class ClientResilientSubclass: ResilientOpenBase {
  var z = 4
}

@allocator(ClientPool)
final class ClientResilientGenericSubclass: ResilientGenericBase<Int> {
  var z = 5
}

@allocator(ClientPool)
final class ClientGeneric<T>: ResilientOpenBase {
  var value: T
  init(_ value: T) { self.value = value }
}

// A class with resiliently-sized fields, whose metadata the runtime
// initializes from a static template.
@allocator(ClientPool)
final class ClientWithResilientField {
  var field = ResilientStruct(1)
}

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  escaped = object
}

final class UnownedHolder<T: AnyObject> {
  unowned let target: T
  init(_ target: T) { self.target = target }
}

/// Checks that the last unowned reference to `make()`'s object frees it.
func checkUnownedDefersFree<T: AnyObject>(
  _ make: () -> T, _ counts: (() -> Void) -> Counts
) {
  var holder: UnownedHolder<T>?
  var object: T? = make()
  let freed = counts {
    holder = UnownedHolder(object!)
    escape(holder)
    object = nil
  }
  expectEqual(freed.deallocations, 0)
  let freedLater = counts {
    holder = nil
    escape(nil)
  }
  expectEqual(freedLater.deallocations, 1)
  _ = holder
  _ = object
}

var tests = TestSuite("ClassAllocatorMultiModule")

tests.test("fragile library classes") {
  expectEqual(counting { escape(LibraryBase()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryDerived()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryDefaulted()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryOptedOut()); escape(nil) },
              Counts(allocations: 0, deallocations: 0))
}

tests.test("inlinable initializers") {
  expectEqual(counting { escape(LibraryBase(inlinable: 3)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryFinal(4)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

tests.test("client subclasses inherit allocators") {
  expectEqual(counting { escape(ClientDerived()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(ClientLeaf()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(ClientDefaulted()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(ClientOptedOut()); escape(nil) },
              Counts(allocations: 0, deallocations: 0))
  expectEqual(counting { escape(makeInLibrary(ClientDerived.self)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

tests.test("client subclass with its own allocator") {
  let counts = countingClient {
    expectEqual(counting { escape(ClientOwn()); escape(nil) },
                Counts(allocations: 0, deallocations: 0))
    expectEqual(counting { escape(makeInLibrary(ClientOwn.self)); escape(nil) },
                Counts(allocations: 0, deallocations: 0))
  }
  expectEqual(counts, Counts(allocations: 2, deallocations: 2))
}

tests.test("resilient library classes") {
  // Internal classes of a library built with library evolution, which the
  // library allocates.
  expectEqual(countingResilient { escape(makeResilientFinal(1)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(countingResilient { escape(makeResilientInner()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(
    countingResilient { escape(makeResilientInnerSub()); escape(nil) },
    Counts(allocations: 1, deallocations: 1))
  expectEqual(
    countingResilient {
      let shape = makeResilientTriangle()
      expectEqual(shape.sides, 3)
      escape(shape)
      escape(nil)
    },
    Counts(allocations: 1, deallocations: 1))
}

tests.test("client classes with resilient ancestry") {
  let counts = countingClient {
    escape(ClientResilientSubclass()); escape(nil)
    escape(ClientResilientGenericSubclass()); escape(nil)
    escape(ClientGeneric(1)); escape(nil)
    escape(ClientGeneric("two")); escape(nil)
    escape(ClientWithResilientField()); escape(nil)
  }
  expectEqual(counts, Counts(allocations: 5, deallocations: 5))
}

tests.test("unowned references to library classes") {
  // The client knows the fragile library's allocators...
  checkUnownedDefersFree({ LibraryFinal(1) }, counting)
  checkUnownedDefersFree({ LibraryBase() }, counting)
  // ...but not the resilient library's: the runtime frees these.
  checkUnownedDefersFree({ makeResilientFinal(1) }, countingResilient)
  checkUnownedDefersFree({ makeResilientInner() }, countingResilient)
  checkUnownedDefersFree({ makeResilientTriangle() }, countingResilient)
}

tests.test("unowned references to client classes") {
  checkUnownedDefersFree({ ClientOwn() }, countingClient)
  checkUnownedDefersFree({ ClientResilientSubclass() }, countingClient)
  checkUnownedDefersFree({ ClientWithResilientField() }, countingClient)
  checkUnownedDefersFree({ ClientGeneric(1.5) }, countingClient)
}

@inline(never)
func read(_ object: LibraryFinal) -> Int { object.x }

@inline(never)
func read(_ object: ClientWithResilientField) -> Int { object.field.x }

/// The counts of `n` stack-promoted objects of a fixed-size class: none if
/// IRGen puts them on the stack, otherwise one allocation and one
/// deallocation each.
func stackPromoted(_ n: Int) -> Counts {
  #if HEAP_FALLBACK
  return Counts(allocations: n, deallocations: n)
  #else
  return isOptimized() ? Counts(allocations: 0, deallocations: 0)
                       : Counts(allocations: n, deallocations: n)
  #endif
}

func isOptimized() -> Bool { !_isDebugAssertConfiguration() }

tests.test("stack-promoted instances") {
  var total = 0
  // An instance of a library class, from its inlined initializer.
  expectEqual(counting {
                for i in 0..<3 {
                  let object = LibraryFinal(i)
                  total &+= read(object)
                }
              },
              stackPromoted(3))
  expectEqual(total, 3)
  // A class whose layout isn't fixed: never on the stack.
  expectEqual(countingClient {
                for _ in 0..<3 {
                  let object = ClientWithResilientField()
                  total &+= read(object)
                }
              },
              Counts(allocations: 3, deallocations: 3))
  expectEqual(total, 6)
}

runAllTests()
