// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plain)
// RUN: %empty-directory(%t/cmo)

// A library built with library evolution whose package classes have
// allocators, and clients in its package.

// Without Package CMO: clients access the library's classes resiliently. The
// library allocates their instances, and the runtime frees them at the last
// unowned release in a client.
// RUN: %target-build-swift-dylib(%t/plain/%target-library-name(PackageLibrary)) -enable-experimental-feature ClassAllocators -target %target-future-triple %S/Inputs/class_allocator_package_library.swift -emit-module -emit-module-path %t/plain/PackageLibrary.swiftmodule -module-name PackageLibrary -package-name ClassAllocatorPackage -enable-library-evolution -O -wmo -swift-version 5 -emit-module-interface-path %t/plain/interface/PackageLibrary.swiftinterface -emit-package-module-interface-path %t/plain/interface/PackageLibrary.package.swiftinterface
// RUN: %target-codesign %t/plain/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/plain -L %t/plain -o %t/plain/main-Onone -Onone %target-rpath(%t/plain)
// RUN: %target-codesign %t/plain/main-Onone
// RUN: %target-run %t/plain/main-Onone %t/plain/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/plain -L %t/plain -o %t/plain/main-O -O %target-rpath(%t/plain)
// RUN: %target-codesign %t/plain/main-O
// RUN: %target-run %t/plain/main-O %t/plain/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/plain -L %t/plain -o %t/plain/main-O-wmo -O -wmo %target-rpath(%t/plain)
// RUN: %target-codesign %t/plain/main-O-wmo
// RUN: %target-run %t/plain/main-O-wmo %t/plain/%target-library-name(PackageLibrary)

// With Package CMO: clients inline the library's allocations and call the
// allocators directly.
// RUN: %target-build-swift-dylib(%t/cmo/%target-library-name(PackageLibrary)) -enable-experimental-feature ClassAllocators -target %target-future-triple %S/Inputs/class_allocator_package_library.swift -emit-module -emit-module-path %t/cmo/PackageLibrary.swiftmodule -module-name PackageLibrary -package-name ClassAllocatorPackage -enable-library-evolution -O -wmo -swift-version 5 -emit-module-interface-path %t/cmo/interface/PackageLibrary.swiftinterface -emit-package-module-interface-path %t/cmo/interface/PackageLibrary.package.swiftinterface -Xfrontend -allow-non-resilient-access -Xfrontend -package-cmo
// RUN: %target-codesign %t/cmo/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/cmo -L %t/cmo -o %t/cmo/main-Onone -Onone %target-rpath(%t/cmo)
// RUN: %target-codesign %t/cmo/main-Onone
// RUN: %target-run %t/cmo/main-Onone %t/cmo/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/cmo -L %t/cmo -o %t/cmo/main-O -O %target-rpath(%t/cmo)
// RUN: %target-codesign %t/cmo/main-O
// RUN: %target-run %t/cmo/main-O %t/cmo/%target-library-name(PackageLibrary)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/cmo -L %t/cmo -o %t/cmo/main-O-wmo -O -wmo %target-rpath(%t/cmo)
// RUN: %target-codesign %t/cmo/main-O-wmo
// RUN: %target-run %t/cmo/main-O-wmo %t/cmo/%target-library-name(PackageLibrary)

// A client that imports the library from its package interface.
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lPackageLibrary -package-name ClassAllocatorPackage -Xfrontend -sil-verify-all -I %t/cmo/interface -L %t/cmo -Xfrontend -experimental-package-interface-load -module-cache-path %t/module-cache -o %t/cmo/main-interface -O %target-rpath(%t/cmo)
// RUN: %target-codesign %t/cmo/main-interface
// RUN: %target-run %t/cmo/main-interface %t/cmo/%target-library-name(PackageLibrary)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import PackageLibrary
import StdlibUnittest
import Synchronization

struct Counts: Equatable {
  var allocations, deallocations: Int
}

/// The allocations and deallocations of `PackagePool` while `body` runs,
/// checking that each deallocation had its allocation's byte count and
/// alignment.
func counting(_ body: () -> Void) -> Counts {
  let allocations = packagePoolAllocations.load(ordering: .relaxed)
  let deallocations = packagePoolDeallocations.load(ordering: .relaxed)
  body()
  expectEqual(packagePoolMismatches.load(ordering: .relaxed), 0)
  return Counts(
    allocations: packagePoolAllocations.load(ordering: .relaxed) - allocations,
    deallocations:
      packagePoolDeallocations.load(ordering: .relaxed) - deallocations)
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

// A subclass of the library's `open` class nested in a package type, which
// has no allocator, may declare the library's package allocator. (A package
// class can't be subclassed outside its module.)
@allocator(PackagePool)
final class ClientSub: Namespace.Base {
  var y = 2
  var vector = SIMD4<Double>(repeating: 1)
}

// Without Package CMO, the client doesn't know the superclass's layout, so a
// subclass's own fields don't tell its size.
@allocator(PackagePool)
final class ClientSmallSub: Namespace.Base {
  var y = 2
}

// A subclass may declare an allocator of its own module.
@allocator(ClientPool)
final class ClientOwn: Namespace.Base {
  var z = 3
}

// A subclass that declares no allocator has none.
final class ClientPlain: Namespace.Base {}

// Fails before its superclass is initialized: the runtime frees it through
// its allocator.
@allocator(PackagePool)
final class ClientFailing: Namespace.Base {
  var y: Int
  @inline(never)
  init(failBeforeSuper: Bool) throws {
    y = 2
    if failBeforeSuper { throw InitFailure() }
    super.init()
  }
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

// Holders of unowned references whose static type is final. The client frees
// the object directly at the last unowned release if the class is its own or
// if it accesses the library without resilience (Package CMO); otherwise the
// runtime frees it.
final class UnownedFinalHolder {
  unowned let target: PackageFinal
  init(_ target: PackageFinal) { self.target = target }
}

final class UnownedSubHolder {
  unowned let target: ClientSub
  init(_ target: ClientSub) { self.target = target }
}

final class UnownedSmallSubHolder {
  unowned let target: ClientSmallSub
  init(_ target: ClientSmallSub) { self.target = target }
}

/// Checks that the last unowned reference to `make()`'s object frees it,
/// through `PackagePool`.
func checkUnownedDefersFree<T: AnyObject, H: AnyObject>(
  _ make: () -> T, _ hold: (T) -> H
) {
  var holder: H?
  var object: T? = make()
  let freed = counting {
    holder = hold(object!)
    escape(holder)
    object = nil
  }
  expectEqual(freed.deallocations, 0)
  let freedLater = counting {
    holder = nil
    escape(nil)
  }
  expectEqual(freedLater.deallocations, 1)
  _ = holder
  _ = object
}

var tests = TestSuite("ClassAllocatorPackageCMO")

tests.test("library instances allocated in the client") {
  // With Package CMO and optimization, the initializer is inlined from the
  // library.
  expectEqual(counting { escape(PackageFinal(1)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  // `open`, so it has no allocator.
  expectEqual(counting { escape(Namespace.Base()); escape(nil) },
              Counts(allocations: 0, deallocations: 0))
  // Allocated by the library, freed by the client's last release.
  expectEqual(counting { escape(makePackageFinal(2)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  // Allocated by the client, freed by the library's deinit.
  var total = 0
  expectEqual(counting {
                for i in 0..<3 {
                  let object = PackageFinal(i)
                  escape(object)
                  total &+= object.x
                }
                escape(nil)
              },
              Counts(allocations: 3, deallocations: 3))
  expectEqual(total, 3)
}

tests.test("client subclass declares the library's allocator") {
  expectEqual(counting { escape(ClientSub()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(ClientSmallSub()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  let counts = countingClient {
    expectEqual(counting { escape(ClientSub()); escape(nil) },
                Counts(allocations: 1, deallocations: 1))
  }
  expectEqual(counts, Counts(allocations: 0, deallocations: 0))
}

tests.test("client subclass with its own allocator") {
  let counts = countingClient {
    expectEqual(counting { escape(ClientOwn()); escape(nil) },
                Counts(allocations: 0, deallocations: 0))
  }
  expectEqual(counts, Counts(allocations: 1, deallocations: 1))
}

tests.test("unowned last release in the library") {
  // The library's `open` class has no allocator, but the other modules of
  // the package can subclass it with one: the library's last unowned release
  // frees an instance through the instance's own allocator.
  for (make, expected) in [
    ({ ClientOwn() as Namespace.Base }, Counts(allocations: 0, deallocations: 1)),
    ({ ClientPlain() as Namespace.Base }, Counts(allocations: 0, deallocations: 0)),
  ] {
    var object: Namespace.Base? = make()
    var holder: AnyObject? = holdUnownedInLibrary(object!)
    let counts = countingClient {
      expectEqual(counting {
                    object = nil
                    holder = nil
                  },
                  Counts(allocations: 0, deallocations: 0))
    }
    expectEqual(counts, expected)
    _ = object
    _ = holder
  }
  checkUnownedDefersFree({ ClientSub() }, holdUnownedInLibrary)
  checkUnownedDefersFree({ ClientSmallSub() }, holdUnownedInLibrary)
}

tests.test("client subclass without an allocator") {
  let counts = countingClient {
    expectEqual(counting { escape(ClientPlain()); escape(nil) },
                Counts(allocations: 0, deallocations: 0))
    // The runtime frees it at the last unowned release, with the system
    // allocator.
    expectEqual(counting {
                  var holder: UnownedHolder<Namespace.Base>? =
                    UnownedHolder(ClientPlain())
                  escape(holder)
                  holder = nil
                  escape(nil)
                  _ = holder
                },
                Counts(allocations: 0, deallocations: 0))
  }
  expectEqual(counts, Counts(allocations: 0, deallocations: 0))
}

tests.test("unowned last release") {
  // The static type is final.
  checkUnownedDefersFree({ PackageFinal(1) }, UnownedFinalHolder.init)
  checkUnownedDefersFree({ makePackageFinal(1) }, UnownedFinalHolder.init)
  checkUnownedDefersFree({ ClientSub() }, UnownedSubHolder.init)
  checkUnownedDefersFree({ ClientSmallSub() }, UnownedSmallSubHolder.init)
  // Through the runtime: the static type isn't final.
  checkUnownedDefersFree({ ClientSub() }, UnownedHolder<Namespace.Base>.init)
  checkUnownedDefersFree({ ClientSmallSub() }, UnownedHolder<Namespace.Base>.init)
}

tests.test("initializers that fail before the object is initialized") {
  expectEqual(counting { _ = try? PackageFinal(fail: true) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { _ = try? ClientFailing(failBeforeSuper: true) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(try? ClientFailing(failBeforeSuper: false)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

runAllTests()
