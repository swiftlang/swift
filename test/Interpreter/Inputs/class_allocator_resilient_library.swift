// A library built with library evolution: only classes that its clients can't
// see may have allocators, and the allocator types may be internal.

import Synchronization

public let resilientPoolAllocations = Atomic<Int>(0)
public let resilientPoolDeallocations = Atomic<Int>(0)

enum ResilientPool: @unsafe ClassAllocator {
  static func allocate(
    byteCount: Int, alignment: Int, for type: AnyClass
  ) -> UnsafeMutableRawPointer {
    resilientPoolAllocations.add(1, ordering: .relaxed)
    return UnsafeMutableRawPointer.allocate(
      byteCount: byteCount, alignment: alignment)
  }

  static func deallocate(
    _ pointer: UnsafeMutableRawPointer, byteCount: Int, alignment: Int,
    for type: AnyClass
  ) {
    resilientPoolDeallocations.add(1, ordering: .relaxed)
    unsafe pointer.deallocate()
  }
}

@allocator(ResilientPool)
final class ResilientFinal {
  var x: Int
  init(_ x: Int) { self.x = x }
}

@allocator(ResilientPool)
class ResilientInner {
  init() {}
}

final class ResilientInnerSub: ResilientInner {}

// A public class can't have an allocator, but its internal subclasses can.
public class ResilientShape {
  public let sides: Int
  init(sides: Int) { self.sides = sides }
}

@allocator(ResilientPool)
final class ResilientTriangle: ResilientShape {
  init() { super.init(sides: 3) }
}

public func makeResilientFinal(_ x: Int) -> AnyObject { ResilientFinal(x) }
public func makeResilientInner() -> AnyObject { ResilientInner() }
public func makeResilientInnerSub() -> AnyObject { ResilientInnerSub() }
public func makeResilientTriangle() -> ResilientShape { ResilientTriangle() }

// Clients may subclass these and give their subclasses allocators.
open class ResilientOpenBase {
  public var base = 1
  public init() {}
}

open class ResilientGenericBase<T> {
  public var value: T?
  public init() {}
}

public struct ResilientStruct {
  public var x: Int
  public init(_ x: Int) { self.x = x }
}
