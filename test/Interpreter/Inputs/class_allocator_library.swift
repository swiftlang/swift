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
    unsafe memory.deallocate()
  }
}

@allocator(LibraryPool)
open class LibraryBase {
  public var x = 1
  public required init() {}
#if FRAGILE
  // Inlined into clients, so they allocate instances of LibraryBase.
  @inlinable
  public init(inlinable x: Int) { self.x = x }
#else
  // Library evolution: inlinable initializers must delegate.
  @inlinable
  public convenience init(inlinable x: Int) {
    self.init()
    self.x = x
  }
#endif
}

open class LibraryDerived: LibraryBase {}

@allocator(LibraryPool)
public final class LibraryFinal {
  public var x: Int
#if FRAGILE
  @inlinable
  public init(_ x: Int) { self.x = x }
#else
  public init() { x = 0 }
  @inlinable
  public convenience init(_ x: Int) {
    self.init()
    self.x = x
  }
#endif
}

@inline(never)
public func makeInLibrary(_ type: LibraryBase.Type) -> LibraryBase {
  return type.init()
}
