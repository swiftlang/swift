// TODO: comment header


/// TODO: docs
@frozen
@available(SwiftStdlib 6.1, *)
public struct UTF8Span: Copyable, ~Escapable, BitwiseCopyable {
  public var unsafeBaseAddress: UnsafeRawPointer?

  /*
   A bit-packed count and flags (such as isASCII)

   ╔═══════╦═════╦═════╦═════╦══════════╦═══════╗
   ║  b63  ║ b62 ║ b61 ║ b60 ║ b59:56   ║ b56:0 ║
   ╠═══════╬═════╬═════╬═════╬══════════╬═══════╣
   ║ ASCII ║ NFC ║ SSC ║ NUL ║ reserved ║ count ║
   ╚═══════╩═════╩═════╩═════╩══════════╩═══════╝

   ASCII means the contents are all-ASCII (<0x7F). 
   NFC means contents are in normal form C for fast comparisons.
   SSC means single-scalar Characters (i.e. grapheme clusters): every
     `Character` holds only a single `Unicode.Scalar`.
   NUL means the contents are a null-terminated C string (that is,
     there is a guranteed, borrowed NULL byte after the end of `count`).

   TODO: NUL means both no-interior and null-terminator, so does this
   mean that String doesn't ever set it because we don't want to scan
   for interior nulls? I think this is the only viable option...

   TODO: Contains-newline would be useful for Regex `.`

   Question: Should we have null-termination support?
             A null-terminated UTF8Span has a NUL byte after its contents
             and contains no interior NULs. How would we ensure the
             NUL byte is exclusively borrowed by us?

   */
  @usableFromInline
  internal var _countAndFlags: UInt64

  @_alwaysEmitIntoClient
  @inline(__always)
  @lifetime(borrow start)
  internal init(
    _unsafeAssumingValidUTF8 start: borrowing UnsafeRawPointer,
    _countAndFlags: UInt64
  ) {
    self.unsafeBaseAddress = copy start
    self._countAndFlags = _countAndFlags

    _invariantCheck()
  }

  // FIXME: we need to make sure ALL API are nil safe, that is they
  // at least check the count first
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    unsafeBaseAddress._unsafelyUnwrappedUnchecked
  }
}

// TODO: init strategy: underscored public that use lifetime annotations

// TODO: try to convert code to be ran on Span instead of URP

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  // TODO: this doesn't need to be underscored, I don't think
  @lifetime(codeUnits)
  public init(
    _validating codeUnits: consuming Span<UInt8>
  ) throws(UTF8.EncodingError) {
    guard let ptr = codeUnits._pointer else {
      self.unsafeBaseAddress = nil
      self._countAndFlags = 0
      return
    }

    // FIXME: handle empty/null span
    let basePtr = codeUnits._start()
    let count = codeUnits._count
    let isASCII = try basePtr._validateUTF8(limitedBy: count)

    self.unsafeBaseAddress = .init(basePtr)
    self._countAndFlags = UInt64(truncatingIfNeeded: count)
    if isASCII {
      _setIsASCII()
    }
    _internalInvariant(self.count == codeUnits.count)
  }

  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafe codeUnits: UnsafeBufferPointer<UInt8>,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   try self.init(
  //     validating: Span(unsafeElements: codeUnits, owner: owner))
  // }

  // // Question: do we want raw versions?
  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafeRaw codeUnits: UnsafeRawBufferPointer,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   try self.init(
  //     validating: Span(unsafeBytes: codeUnits, owner: owner))
  // }

  // // Question: do we want separate count versions?
  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafeStart start: UnsafePointer<UInt8>,
  //   count: Int,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   try self.init(
  //     validating: Span(unsafeStart: start, count: count, owner: owner))
  // }

  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafeStart start: UnsafeRawPointer,
  //   count: Int,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   try self.init(
  //     validating: Span(unsafeStart: start, byteCount: count, owner: owner))
  // }

  // // Question: Do we do a raw version? String doesn't have one
  // // Also, should we do a UnsafePointer<UInt8> version, it's
  // // annoying to not have one sometimes...?
  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafeRawCString nullTerminatedUTF8: UnsafeRawPointer,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   // TODO: is there a better way?
  //   try self.init(
  //     validatingUnsafeCString: nullTerminatedUTF8.assumingMemoryBound(
  //       to: CChar.self
  //     ),
  //     owner: owner)
  //   _internalInvariant(self.isNullTerminatedCString)
  // }

  // @_alwaysEmitIntoClient
  // public init<Owner: ~Copyable & ~Escapable>(
  //   validatingUnsafeCString nullTerminatedUTF8: UnsafePointer<CChar>,
  //   owner: borrowing Owner
  // ) throws(UTF8.EncodingError) -> dependsOn(owner) Self {
  //   let len = UTF8._nullCodeUnitOffset(in: nullTerminatedUTF8)
  //   try self.init(
  //     validatingUnsafeStart: UnsafeRawPointer(nullTerminatedUTF8),
  //     count: len,
  //     owner: owner)
  //   self._setIsNullTerminatedCString(true)
  // }
}


// MARK: Canonical comparison

@_unavailableInEmbedded
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  // HACK: working around lack of internals
  internal var _str: String { _start()._str(0..<count) }

  /// Whether `self` is equivalent to `other` under Unicode Canonical
  /// Equivalence.
  public func isCanonicallyEquivalent(
    to other: UTF8Span
  ) -> Bool {
    self._str == other._str
  }

  /// Whether `self` orders less than `other` under Unicode Canonical 
  /// Equivalence using normalized code-unit order (in NFC).
  public func isCanonicallyLessThan(
    _ other: UTF8Span
  ) -> Bool {
    self._str < other._str
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
  borrowing public func withUnsafeBufferPointer<
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
  @_alwaysEmitIntoClient @inline(__always)
  internal func _invariantCheck() {
#if DEBUG
    if isNullTerminatedCString {
      _internalInvariant(
        _start().load(fromByteOffset: count, as: UInt8.self) == 0)
      // TODO: byte scan for no interior nulls...
    }
#endif
  }
}

#if false
extension RawSpan {
  public func parseUTF8(from start: Int, length: Int) throws -> UTF8Span {
    let span = self[
      uncheckedOffsets: start ..< start &+ length
    ].view(as: UInt8.self)
    return try UTF8Span(validating: span)
  }

  // TODO: Below are contingent on how we want to handle NUL-termination
  public func parseNullTerminatedUTF8() throws -> UTF8Span {
    fatalError()
  }
}

// TODO: Below is contingent on a Cursor or Iterator type
extension RawSpan.Cursor {
  public mutating func parseUTF8(length: Int) throws -> UTF8Span {
    fatalError()
  }
  public mutating func parseNullTerminatedUTF8() throws -> UTF8Span {
    fatalError()
  }
}
#endif

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  static func ~=(_ lhs: UTF8Span, _ rhs: StaticString) -> Bool {
    fatalError()
  }

  // Not doing == between two UTFSpan, as pointerness 
  // Note: avove might not be possible 
}


// TODO: cString var, or something like that
