/// A value at a stable memory location.
@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
public protocol Anchored {
  associatedtype Value

  static var defaultInitialValue: Value { get }
  init(at address: UnsafeMutablePointer<Value>, in anchor: AnyObject)
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
@propertyWrapper
@frozen
public struct Anchoring<Thing: Anchored> {
  @usableFromInline
  internal var _storage: Thing.Value

  public init() {
    self._storage = Thing.defaultInitialValue
  }

  public init(_ value: Thing.Value) {
    self._storage = value
  }

  public var wrappedValue: Thing {
    get { fatalError("Unimplemented") }
    set { fatalError("FIXME") }
  }

  public static subscript<Anchor: AnyObject>(
    _enclosingInstance anchor: Anchor,
    wrapped wrappedKeyPath: ReferenceWritableKeyPath<Anchor, Thing>,
    storage storageKeyPath: ReferenceWritableKeyPath<Anchor, Self>
  ) -> Thing {
    @_transparent _read {
      let keyPath = storageKeyPath.appending(path: \._storage)
      let p = MemoryLayout.unsafeAddress(of: keyPath, in: anchor)!
      yield Thing(at: p, in: anchor)
    }
    set {
      fatalError("FIXME")
    }
  }
}
