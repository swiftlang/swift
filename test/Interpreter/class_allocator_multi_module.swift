// RUN: %empty-directory(%t)

// Library evolution, so that clients see the library's classes as resilient.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -enable-experimental-feature ClassAllocators -target %target-future-triple -enable-library-evolution %S/Inputs/class_allocator_library.swift -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library -O
// RUN: %target-codesign %t/%target-library-name(Library)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lLibrary -I %t -L %t -o %t/main-resilient -O %target-rpath(%t)
// RUN: %target-codesign %t/main-resilient
// RUN: %target-run %t/main-resilient %t/%target-library-name(Library)

// The same, without library evolution and without optimization.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -enable-experimental-feature ClassAllocators -target %target-future-triple %S/Inputs/class_allocator_library.swift -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library -Onone -D FRAGILE
// RUN: %target-codesign %t/%target-library-name(Library)
// RUN: %target-build-swift -enable-experimental-feature ClassAllocators -target %target-future-triple %s -lLibrary -I %t -L %t -o %t/main-fragile -Onone %target-rpath(%t)
// RUN: %target-codesign %t/main-fragile
// RUN: %target-run %t/main-fragile %t/%target-library-name(Library)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ClassAllocators
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Library
import StdlibUnittest

// Subclasses in another module inherit the allocator.
class ClientDerived: LibraryDerived {
  var y = 2
}

final class ClientLeaf: LibraryBase {}

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

nonisolated(unsafe) var escaped: AnyObject?

@inline(never)
func escape(_ object: AnyObject?) {
  unsafe escaped = object
}

var tests = TestSuite("ClassAllocatorMultiModule")

tests.test("library classes") {
  expectEqual(counting { escape(LibraryBase()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryDerived()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

tests.test("inlinable initializers") {
  expectEqual(counting { escape(LibraryBase(inlinable: 3)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(LibraryFinal(4)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

tests.test("client subclasses") {
  expectEqual(counting { escape(ClientDerived()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
  expectEqual(counting { escape(ClientLeaf()); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

tests.test("client subclasses allocated by the library") {
  expectEqual(counting { escape(makeInLibrary(ClientDerived.self)); escape(nil) },
              Counts(allocations: 1, deallocations: 1))
}

runAllTests()
