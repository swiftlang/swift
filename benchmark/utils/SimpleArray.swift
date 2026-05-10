@usableFromInline
@frozen
struct Header {
  @usableFromInline
  let capacity: Int

  @usableFromInline
  var count: Int
}

public final class SimpleArray<T> {
  @usableFromInline let storage: ManagedBuffer<Header, T>

  @_alwaysEmitIntoClient
  @inline(__always)
  @inlinable
  var count: Int {
    get {
      return self.storage.withUnsafeMutablePointerToHeader { return $0.pointee.count }
    }
    set {
      return self.storage.withUnsafeMutablePointerToHeader { $0.pointee.count = newValue }
    }
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  @inlinable
  var capacity: Int {
    return self.storage.withUnsafeMutablePointerToHeader { return $0.pointee.capacity }
  }

  public init(capacity: Int) {
    self.storage = .create(minimumCapacity: capacity) { _ in
      return Header(capacity: capacity, count: 0)
    }
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  @inlinable
  func append_internal(_ element: __owned T) {
    guard count < capacity else {
      fatalError("Array is full")
    }
    storage.withUnsafeMutablePointerToElements { ($0 + count).initialize(to: element) }
    count += 1
  }

  @inline(never)
  @_effects(notEscaping self.**)
  @_specialize(exported: true, where @_noMetadata T : _Class)
  public func append(_ element: __owned T) {
    append_internal(element)
  }

  @inline(never)
  @inlinable
  @_effects(notEscaping self.**)
  public func append2(_ element: __owned T) {
    append_internal(element)
  }

  @inline(__always)
  @inlinable
  @_effects(notEscaping self.**)
  public func append3(_ element: __owned T) {
    append_internal(element)
  }

  @inline(never)
  @_effects(notEscaping self.**)
  public func append4(_ element: __owned T) {
    append_internal(element)
  }

  public func clear() {
    // only reset the count to avoid measuring deinitialization
    // overhead in benchmark
    self.count = 0
  }
}