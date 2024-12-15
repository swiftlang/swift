// TODO: comment header


/// TODO: docs
@frozen
@available(SwiftStdlib 6.1, *)
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
  @lifetime(borrow start)
  internal init(
    _unsafeAssumingValidUTF8 start: borrowing UnsafeRawPointer,
    _countAndFlags: UInt64
  ) {
    self._unsafeBaseAddress = copy start
    self._countAndFlags = _countAndFlags

    _invariantCheck()
  }

  // FIXME: we need to make sure ALL API are nil safe, that is they
  // at least check the count first
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    _unsafeBaseAddress._unsafelyUnwrappedUnchecked
  }

  // HACK: working around lack of internal plumbing work
  internal var _str: String { _start()._str(0..<count) }
}

// TODO: try to convert code to be ran on Span instead of URP

@available(SwiftStdlib 6.1, *)
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
    guard let ptr = codeUnits._pointer else {
      self._unsafeBaseAddress = nil
      self._countAndFlags = 0
      return
    }

    // FIXME: handle empty/null span
    let basePtr = codeUnits._start()
    let count = codeUnits._count
    let isASCII = try basePtr._validateUTF8(limitedBy: count)

    self._unsafeBaseAddress = .init(basePtr)
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
    guard let ptr = codeUnits._pointer else {
      self._unsafeBaseAddress = nil
      self._countAndFlags = 0
      return
    }

    self._unsafeBaseAddress = codeUnits._start()
    self._countAndFlags = UInt64(truncatingIfNeeded: codeUnits.count)
    if isKnownASCII {
      _setIsASCII()
    }
    if isKnownNFC {
      _setIsNFC()
    }
    _internalInvariant(self.count == codeUnits.count)
  }
}


// MARK: String


@available(SwiftStdlib 6.1, *)
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
    try body(_start()._ubp(0..<count))
  }

  // TODO: withSpan or similar?
}

// MARK: Internals

@available(SwiftStdlib 6.1, *)
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

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  public var isEmpty: Bool {
    self.count == 0
  }

  public var span: Span<UInt8> {
    @lifetime(copy self)
    get {
      Span(_unchecked: _unsafeBaseAddress, count: self.count)
    }
  }


}

func UNSUPPORTED(_ message: String) -> Never {
  fatalError("UNSUPPORTED: \(message)")
}

// TODO(toolchain): decide if we rebase on top of Guillaume's work
@available(SwiftStdlib 6.1, *)
extension String {
  public var utf8Span: UTF8Span {
    UNSUPPORTED("utf8Span property pending compiler fixes")
  }
}

@available(SwiftStdlib 6.1, *)
extension Substring {
  public var utf8Span: UTF8Span {
    UNSUPPORTED("utf8Span property pending compiler fixes")
  }
}




