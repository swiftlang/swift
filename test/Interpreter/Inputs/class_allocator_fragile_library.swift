// A library built without library evolution: clients subclass its classes,
// inherit their allocators and call them directly. Built with
// `-default-class-allocator Library.LibraryPool`.

import Synchronization

public let libraryPoolAllocations = Atomic<Int>(0)
public let libraryPoolDeallocations = Atomic<Int>(0)

public enum LibraryPool: @unsafe ClassAllocator {
  public static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    libraryPoolAllocations.add(1, ordering: .relaxed)
    let memory = UnsafeMutableRawPointer.allocate(
      byteCount: byteCount + 16, alignment: max(alignment, 16))
    unsafe memory.storeBytes(of: 0x4c6962, as: Int.self)
    return unsafe memory + 16
  }

  public static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    libraryPoolDeallocations.add(1, ordering: .relaxed)
    let memory = unsafe pointer - 16
    precondition(unsafe memory.load(as: Int.self) == 0x4c6962,
                 "freed memory that LibraryPool did not allocate")
    unsafe memory.storeBytes(of: 0, as: Int.self)
    unsafe memory.deallocate()
  }
}

@allocator(LibraryPool)
open class LibraryBase {
  public var x = 1
  public required init() {}

  // Inlined into clients, which then allocate instances of LibraryBase.
  @inlinable
  public init(inlinable x: Int) { self.x = x }
}

open class LibraryDerived: LibraryBase {}

@allocator(LibraryPool)
public final class LibraryFinal {
  public var x: Int
  @inlinable
  public init(_ x: Int) { self.x = x }
}

// Gets LibraryPool from -default-class-allocator.
open class LibraryDefaulted {
  public init() {}
}

// Opts out of -default-class-allocator.
@allocator(SystemClassAllocator)
open class LibraryOptedOut {
  public init() {}
}

@inline(never)
public func makeInLibrary(_ type: LibraryBase.Type) -> LibraryBase {
  return type.init()
}
