// A library built with library evolution, whose internal actors have
// allocators.

import Synchronization

public let libraryActorAllocations = Atomic<Int>(0)
public let libraryActorDeallocations = Atomic<Int>(0)

enum LibraryActorPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    libraryActorAllocations.add(1, ordering: .relaxed)
    return UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    libraryActorDeallocations.add(1, ordering: .relaxed)
    unsafe pointer.deallocate()
  }
}

public protocol Counter: Actor {
  func increment() -> Int
}

@allocator(LibraryActorPool)
actor LibraryCounter: Counter {
  var count = 0
  func increment() -> Int {
    count += 1
    return count
  }
}

public func makeLibraryCounter() -> any Counter { LibraryCounter() }
