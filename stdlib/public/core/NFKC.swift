
//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Unicode.NormalizedScalars where Source: Sequence<Unicode.Scalar> {

  /// The contents of the source sequence, in Normalization Form KC.
  ///
  /// Normalization to NFKC does _not_ preserve canonical equivalence.
  ///
  @inlinable
  public var nfkc: NFKC {
    Unicode.NormalizedScalars.NFKC(source)
  }

  /// The contents of the source sequence, in Normalization Form KC.
  ///
  /// Normalization to NFKC does _not_ preserve canonical equivalence.
  ///
  @frozen
  public struct NFKC {

    @usableFromInline
    internal let source: Source

    @inlinable
    internal init(_ source: Source) {
      self.source = source
    }
  }
}

extension Unicode.NormalizedScalars.NFKC: Sequence {

  @inlinable
  public consuming func makeIterator() -> Iterator {
    Iterator(source: source.makeIterator())
  }

  @frozen
  public struct Iterator: IteratorProtocol {

    public var source: Source.Iterator

    @usableFromInline
    internal var normalizer: Unicode.NFKCNormalizer

    @inlinable
    internal init(source: Source.Iterator) {
      self.source = source
      self.normalizer = Unicode.NFKCNormalizer()
    }

    @inlinable
    public mutating func next() -> Unicode.Scalar? {
      normalizer.resume(consuming: &source) ?? normalizer.flush()
    }
  }
}

extension Unicode.NormalizedScalars.NFKC: Sendable where Source: Sendable {}
extension Unicode.NormalizedScalars.NFKC.Iterator: Sendable where Source.Iterator: Sendable {}

extension Unicode {

  /// A stateful normalizer, producing a single logical stream
  /// of normalized text from chunked inputs.
  ///
  /// To use the normalizer, first create an instance.
  /// Next, feed it a chunk of a text stream using the `resume(consuming:)`
  /// function. The normalizer will consume from the stream and buffer
  /// it as needed, so continue feeding the same source until
  /// it returns `nil`, indicating that the source was exhausted.
  ///
  /// ```swift
  /// var normalizer = Unicode.NFKCNormalizer()
  ///
  /// var input: some IteratorProtocol<Unicode.Scalar> = ...
  /// while let scalar = normalizer.resume(consuming: &input) {
  ///   print(scalar)
  /// }
  ///
  /// // assert(input.next() == nil)
  /// ```
  ///
  /// You may continue consuming sources until you reach the end
  /// of the logical text stream. Once you reach the end,
  /// call `flush()` to drain any remaining content
  /// from the normalizer's buffers.
  ///
  /// ```swift
  /// while let scalar = normalizer.flush() {
  ///   print(scalar)
  /// }
  /// ```
  ///
  /// The chunks of input text do not need to be aligned on any normalization
  /// boundary. The normalizer state has value semantics, so it is possible
  /// to copy and store and is inherently thread-safe.
  ///
  public struct NFKCNormalizer: Sendable {

    // FIXME: Factor out canonical composition and apply it directly to the output of the NFKD normalizer.
    // `toNFC(toNFKD(source))` gives the correct result but is suboptimal because it decomposes the NFKD result again.

    internal var _nfkd = NFKDNormalizer()
    internal var _nfc = NFCNormalizer()
    internal var _isFlushing = false

    /// Creates a new normalizer.
    ///
    public init() { }

    public mutating func reset(maximumCapacity: Int = 64) {
      _nfkd.reset(maximumCapacity: maximumCapacity)
      _nfc.reset(maximumCapacity: maximumCapacity)
      _isFlushing = false
    }

    /// Resume normalizing the text stream.
    ///
    /// Each call to `resume` returns the next scalar in the normalized output,
    /// consuming elements from the given source as necessary.
    ///
    /// If the normalizer returns `nil`, the source was exhausted.
    /// One a source is exhausted, you may:
    ///
    /// - Call `resume` again some time later with a different source
    ///   to continue processing the same logical text stream, or
    ///
    /// - Call `flush` in order to mark the end of the stream
    ///   and consume data remaining in the normalizer's internal buffers.
    ///
    /// Typical usage looks like the following:
    ///
    /// ```swift
    /// var normalizer = Unicode.NFKCNormalizer()
    ///
    /// var input: some IteratorProtocol<Unicode.Scalar> = ...
    /// while let scalar = normalizer.resume(consuming: &input) {
    ///   print(scalar)
    /// }
    ///
    /// // We could resume again, consuming from another input here.
    /// // Finally, when we are done consuming inputs:
    ///
    /// while let scalar = normalizer.flush() {
    ///   print(scalar)
    /// }
    /// ```
    ///
    /// The normalizer consumes data from the source as needed,
    /// meaning even if a call to `resume` returns a value,
    /// that value may have come from the normalizer's internal buffers
    /// without consuming the input source at all.
    ///
    /// Be careful to ensure each input source has been fully consumed
    /// before moving on to the next source (marked by `resume` returning `nil`).
    ///
    @inlinable
    public mutating func resume(
      consuming source: inout some IteratorProtocol<Unicode.Scalar>
    ) -> Unicode.Scalar? {
      resume(consuming: { source.next() })
    }

    // ABI barrier for resume(consuming: inout some IteratorProtocol<Unicode.Scalar>).
    @usableFromInline
    internal mutating func resume(
      consuming nextFromSource: () -> Unicode.Scalar?
    ) -> Unicode.Scalar? {
      guard !_isFlushing else { return nil }
      return _nfc.resume { _nfkd.resume { nextFromSource() } }
    }

    /// Marks the end of the text stream and
    /// returns the next scalar from the normalizer's internal buffer.
    ///
    /// Once you have finished feeding input data to the normalizer,
    /// call `flush` until it returns `nil`.
    ///
    /// ```swift
    /// while let scalar = normalizer.flush() {
    ///   print(scalar)
    /// }
    /// ```
    ///
    /// After calling `flush`, all future calls to `resume`
    /// will immediately return `nil` without consuming from its source.
    /// This allows optional chaining to be used to
    /// fully normalize a stream:
    ///
    /// ```swift
    /// // Normalize the concatenation of inputA and inputB
    ///
    /// while let scalar =
    ///   normalizer.resume(consuming: &inputA) ??
    ///   normalizer.resume(consuming: &inputB) ??
    ///   normalizer.flush()
    /// {
    ///   print(scalar)
    /// }
    /// ```
    ///
    public mutating func flush() -> Unicode.Scalar? {
      _isFlushing = true
      return _nfc.resume { _nfkd.flush() } ?? _nfc.flush()
    }
  }
}
