// A library built with library evolution, with or without Package CMO: its
// package classes may have allocators. Its clients in the package call them
// directly only with Package CMO, which relies on their being built with it.
// Its `open` class nested in a package type can't have one, but the clients'
// subclasses may declare the library's package allocator.

import Synchronization

package let packagePoolAllocations = Atomic<Int>(0)
package let packagePoolDeallocations = Atomic<Int>(0)
/// Deallocations whose byte count or alignment wasn't the allocation's, or
/// of memory that the pool didn't allocate.
package let packagePoolMismatches = Atomic<Int>(0)

let packagePoolLive = Mutex<[UnsafeMutableRawPointer: (Int, Int)]>([:])

package enum PackagePool: @unsafe ClassAllocator {
  package static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    packagePoolAllocations.add(1, ordering: .relaxed)
    let pointer = UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
    packagePoolLive.withLock { $0[pointer] = (byteCount, alignment) }
    return pointer
  }

  package static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    packagePoolDeallocations.add(1, ordering: .relaxed)
    let entry = packagePoolLive.withLock { $0.removeValue(forKey: pointer) }
    if entry == nil || entry! != (byteCount, alignment) {
      packagePoolMismatches.add(1, ordering: .relaxed)
    }
    unsafe pointer.deallocate()
  }
}

package struct InitFailure: Error {
  package init() {}
}

@allocator(PackagePool)
package final class PackageFinal {
  package var x: Int
  package init(_ x: Int) { self.x = x }
  // Fails before `x` is initialized.
  @inline(never)
  package init(fail: Bool) throws {
    if fail { throw InitFailure() }
    x = 1
  }
}

package enum Namespace {
  // Visible only in the package, but subclassable by its other modules, so
  // it can't have an allocator (in a module built with library evolution).
  @allocator(SystemClassAllocator)
  open class Base {
    package var base = 1
    package init() {}
  }
}

/// Allocated by the library.
@inline(never)
package func makePackageFinal(_ x: Int) -> PackageFinal { PackageFinal(x) }

/// Holds an unowned reference to `object` in the library, which may be an
/// instance of a client's subclass with an allocator: the last unowned
/// release, in the library, must free it through the instance's own
/// allocator.
@inline(never)
package func holdUnownedInLibrary(_ object: Namespace.Base) -> AnyObject {
  LibraryUnownedHolder(object)
}

final class LibraryUnownedHolder {
  unowned let target: Namespace.Base
  init(_ target: Namespace.Base) { self.target = target }
}
