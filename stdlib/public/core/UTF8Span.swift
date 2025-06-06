// TODO: comment header


/// TODO: docs
@frozen
@safe
@available(SwiftStdlib 6.2, *)
public struct UTF8Span: Copyable, ~Escapable, BitwiseCopyable {
  @usableFromInline
  internal var _unsafeBaseAddress: UnsafeRawPointer?

  /*
   A bit-packed count and flags (such as isASCII)

   ╔═══════╦═════╦══════════╦═══════╗
   ║  b63  ║ b62 ║ b61:56   ║ b56:0 ║
   ╠═══════╬═════╬══════════╬═══════╣
   ║ ASCII ║ NFC ║ reserved ║ count ║
   ╚═══════╩═════╩══════════╩═══════╝

   ASCII means the contents are known to be all-ASCII (<0x7F).
   NFC means contents are known to be in normal form C for fast comparisons.
   */
  @usableFromInline
  internal var _countAndFlags: UInt64

  // @_alwaysEmitIntoClient
  @inline(__always)
  @lifetime(borrow start) // TODO: borrow or copy?
  internal init(
    _unsafeAssumingValidUTF8 start: borrowing UnsafeRawPointer,
    _countAndFlags: UInt64
  ) {
    unsafe self._unsafeBaseAddress = copy start
    self._countAndFlags = _countAndFlags

    _invariantCheck()
  }

  /// Creates a UTF8Span, bypassing safety and security checks. The caller
  /// must guarantee that `codeUnits` contains validly-encoded UTF-8, or else
  /// undefined behavior may result upon use. If `isKnownASCII: true is
  /// passed`, the contents must be ASCII, or else undefined behavior may
  /// result upon use.
  @unsafe
  @lifetime(copy codeUnits)
  public init(
    unchecked codeUnits: Span<UInt8>,
    isKnownASCII: Bool = false
  ) {
    self.init(
      _uncheckedAssumingValidUTF8: codeUnits,
      isKnownASCII: isKnownASCII,
      isKnownNFC: false
    )
  }

  // FIXME: we need to make sure ALL API are nil safe, that is they
  // at least check the count first
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    unsafe _unsafeBaseAddress._unsafelyUnwrappedUnchecked
  }
}

// TODO: try to convert code to be ran on Span instead of URP

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Creates a UTF8Span containing `codeUnits`. Validates that the input is
  /// valid UTF-8, otherwise throws an error.
  ///
  /// The resulting UTF8Span has the same lifetime constraints as `codeUnits`.
  @lifetime(copy codeUnits)
  public init(
    validating codeUnits: consuming Span<UInt8>
  ) throws(UTF8.ValidationError) {
    try self.init(_validating: codeUnits)
  }

  // TODO: this doesn't need to be underscored, I don't think
  @lifetime(copy codeUnits)
  internal init(
    _validating codeUnits: consuming Span<UInt8>
  ) throws(UTF8.ValidationError) {
    guard let basePtr = unsafe codeUnits._pointer else {
      unsafe self._unsafeBaseAddress = nil
      self._countAndFlags = 0
      return
    }

    let count = codeUnits._count
    let isASCII = unsafe try basePtr._validateUTF8(limitedBy: count)

    unsafe self._unsafeBaseAddress = .init(basePtr)
    self._countAndFlags = UInt64(truncatingIfNeeded: count)
    if isASCII {
      _setIsASCII()
    }
    _internalInvariant(self.count == codeUnits.count)
  }

  // TODO: SPI?
  @lifetime(copy codeUnits)
  internal init(
    _uncheckedAssumingValidUTF8 codeUnits: consuming Span<UInt8>,
    isKnownASCII: Bool,
    isKnownNFC: Bool
  ) {
    guard let ptr = unsafe codeUnits._pointer else {
      unsafe self._unsafeBaseAddress = nil
      self._countAndFlags = 0
      return
    }

    unsafe self._unsafeBaseAddress = ptr
    self._countAndFlags = UInt64(truncatingIfNeeded: codeUnits.count)
    if isKnownASCII {
      _setIsASCII()
    }
    if isKnownNFC {
      _setIsNFC()
    }
    _internalInvariant(self.count == codeUnits.count)
  }

  // HACK: working around lack of internal plumbing work
  internal var _str: String { unsafe _start()._str(0..<count) }
}


// MARK: String


@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Calls a closure with a pointer to the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBufferPointer(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter
  ///   that points to the viewed contiguous storage. If `body` has
  ///   a return value, that value is also used as the return value
  ///   for the `withUnsafeBufferPointer(_:)` method. The closure's
  ///   parameter is valid only for the duration of its execution.
  /// - Returns: The return value of the `body` closure parameter.
  @_alwaysEmitIntoClient
  borrowing public func _withUnsafeBufferPointer<
    E: Error, Result: ~Copyable //& ~Escapable
  >(
    _ body: (_ buffer: /*borrowing*/ UnsafeBufferPointer<UInt8>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(_start()._ubp(0..<count))
  }

  // TODO: withSpan or similar?
}

// MARK: Internals

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
#if !INTERNAL_CHECKS_ENABLED
  @inline(__always) internal func _invariantCheck() {}
#else
  @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    // TODO: validate the UTF-8 as an assertion (and isASCII)
  }
#endif
}

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  public var isEmpty: Bool {
    self.count == 0
  }

  public var span: Span<UInt8> {
    @lifetime(copy self)
    get {
      let newSpan = unsafe Span<UInt8>(_unchecked: _unsafeBaseAddress, count: self.count)
      return unsafe _overrideLifetime(newSpan, copying: self)
    }
  }


}

// TODO(toolchain): decide if we rebase on top of Guillaume's work
extension String {

  @available(SwiftStdlib 6.2, *)
  public init(copying codeUnits: UTF8Span) {
    let isASCII = codeUnits.isKnownASCII
    self = unsafe codeUnits._withUnsafeBufferPointer { bufPtr in
      unsafe String._uncheckedFromUTF8(bufPtr, isASCII: isASCII)
    }
  }

  @available(SwiftStdlib 6.2, *)
  private var _span: Span<UTF8.CodeUnit> {
    @lifetime(borrow self)
    borrowing get {
#if _runtime(_ObjC)
      // handle non-UTF8 Objective-C bridging cases here
      if !_guts.isFastUTF8, _guts._object.hasObjCBridgeableObject {
        let storage = _guts._getOrAllocateAssociatedStorage()
        let (start, count) = unsafe (storage.start, storage.count)
        let span = unsafe Span(_unsafeStart: start, count: count)
        return unsafe _overrideLifetime(span, borrowing: self)
      }
#endif
      let count = _guts.count
      if _guts.isSmall {
        let a = Builtin.addressOfBorrow(self)
        let address = unsafe UnsafePointer<UTF8.CodeUnit>(a)
        let span = unsafe Span(_unsafeStart: address, count: count)
        return unsafe _overrideLifetime(span, borrowing: self)
      }
      let isFastUTF8 = _guts.isFastUTF8
      _precondition(isFastUTF8, "String must be contiguous UTF8")
      let buffer = unsafe _guts._object.fastUTF8
      let span = unsafe Span(_unsafeElements: buffer)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  /// A UTF8span over the code units that make up this string.
  ///
  /// - Note: In the case of bridged UTF16 String instances (on Apple
  /// platforms,) this property transcodes the code units the first time
  /// it is called. The transcoded buffer is cached, and subsequent calls
  /// to `span` can reuse the buffer.
  ///
  ///  Returns: a `UTF8Span` over the code units of this String.
  ///
  ///  Complexity: O(1) for native UTF8 Strings,
  ///    amortized O(1) for bridged UTF16 Strings.
  @available(SwiftStdlib 6.2, *)
  public var utf8Span: UTF8Span {
    @lifetime(borrow self)
    borrowing get {
      unsafe UTF8Span(unchecked: _span, isKnownASCII: _guts.isASCII)
    }
  }
}

extension Substring {
  @available(SwiftStdlib 6.2, *)
  public var utf8Span: UTF8Span {
    @lifetime(borrow self)
    borrowing get {
      let isKnownASCII = base._guts.isASCII
      let utf8 = self.utf8
      let span = utf8.span
      let result = unsafe UTF8Span(
        unchecked: span,
        isKnownASCII: isKnownASCII)
      return unsafe _overrideLifetime(result, borrowing: self)
    }
  }
}




