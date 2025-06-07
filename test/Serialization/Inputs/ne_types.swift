@_addressableForDependencies
public struct Something {
  public struct View<T: BitwiseCopyable> : ~Copyable, ~Escapable {
    var ptr: UnsafeBufferPointer<T>
    
    @lifetime(borrow ptr)
    public init(ptr: borrowing UnsafeBufferPointer<T>) {
      self.ptr = copy ptr
    }
    
    public var span: Span<T> {
      @lifetime(borrow self)
      borrowing get {
        Span(_unsafeElements: ptr)
      }
    }
  }
  
  public struct MutableView<T: BitwiseCopyable> : ~Copyable, ~Escapable {
    var ptr: UnsafeMutableBufferPointer<T>
    
    @lifetime(borrow ptr)
    public init(ptr: borrowing UnsafeMutableBufferPointer<T>) {
      self.ptr = copy ptr
    }
    
    public var mutableSpan: MutableSpan<T> {
      @lifetime(&self)
      mutating get {
        MutableSpan(_unsafeElements: ptr)
      }
    }
  }
  
  var ptr: UnsafeMutableRawBufferPointer
  public init(ptr: UnsafeMutableRawBufferPointer) {
    self.ptr = ptr
  }
  
  @lifetime(borrow self)
  public func view<T>(of type: T.Type = T.self) -> View<T> {
    let tp = ptr.assumingMemoryBound(to: T.self)
    return __overrideLifetime(View(ptr: .init(tp)), borrowing: self)
  }
  
  @lifetime(&self)
  public mutating func mutableView<T>(of type: T.Type = T.self) -> MutableView<T> {
    let tp = ptr.assumingMemoryBound(to: T.self)
    return __overrideLifetime(MutableView(ptr: tp), mutating: &self)
  }
}

@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(borrow source)
public func __overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, borrowing source: borrowing U
) -> T {
  dependent
}

/// Unsafely discard any lifetime dependency on the `dependent` argument. Return
/// a value identical to `dependent` that inherits all lifetime dependencies from
/// the `source` argument.
@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(copy source)
public func __overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, copying source: borrowing U
) -> T {
  dependent
}

/// Unsafely discard any lifetime dependency on the `dependent` argument.
/// Return a value identical to `dependent` with a lifetime dependency
/// on the caller's exclusive borrow scope of the `source` argument.
@unsafe
@_unsafeNonescapableResult
@_alwaysEmitIntoClient
@_transparent
@lifetime(&source)
public func __overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T,
  mutating source: inout U
) -> T {
  dependent
}

