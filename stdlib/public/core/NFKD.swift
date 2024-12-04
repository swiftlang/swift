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

  /// The contents of the source sequence, in Normalization Form KD.
  ///
  /// Normalization to NFKD does _not_ preserve canonical equivalence.
  ///
  @inlinable
  public var nfkd: NFKD {
    Unicode.NormalizedScalars.NFKD(source)
  }

  /// The contents of the source sequence, in Normalization Form KD.
  ///
  /// Normalization to NFKD does _not_ preserve canonical equivalence.
  ///
  @frozen
  public struct NFKD {

    @usableFromInline
    internal let source: Source

    @inlinable
    internal init(_ source: Source) {
      self.source = source
    }
  }
}

extension Unicode.NormalizedScalars.NFKD: Sequence {

  @inlinable
  public consuming func makeIterator() -> Iterator {
    Iterator(source: source.makeIterator())
  }

  @frozen
  public struct Iterator: IteratorProtocol {

    public var source: Source.Iterator

    @usableFromInline
    internal var normalizer: Unicode.NFKDNormalizer

    @inlinable
    internal init(source: Source.Iterator) {
      self.source = source
      self.normalizer = Unicode.NFKDNormalizer()
    }

    @inlinable
    public mutating func next() -> Unicode.Scalar? {
      normalizer.resume(consuming: &source) ?? normalizer.flush()
    }
  }
}

extension Unicode.NormalizedScalars.NFKD: Sendable where Source: Sendable {}
extension Unicode.NormalizedScalars.NFKD.Iterator: Sendable where Source.Iterator: Sendable {}

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
  /// var normalizer = Unicode.NFKDNormalizer()
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
  public struct NFKDNormalizer: Sendable {

    internal enum State {
      case emittingSegment
      case consuming
      case terminated
    }

    internal var state = State.consuming

    internal var buffer = Unicode._CompatibilityNormalizationBuffer()
    internal var pendingStarter = Optional<ScalarAndCompatNormData>.none
    internal var bufferIsSorted = false

    /// Creates a new normalizer.
    ///
    public init() { }

    public mutating func reset(maximumCapacity: Int = 64) {
      state = .consuming
      buffer.reset(maximumCapacity: maximumCapacity)
      pendingStarter = .none
      bufferIsSorted = false
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
    /// var normalizer = Unicode.NFKDNormalizer()
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
      _resume(consuming: nextFromSource)?.scalar
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
      _flush()?.scalar
    }
  }
}

extension Unicode.NFKDNormalizer {

  @inline(never)
  internal mutating func _resume(
    consuming nextFromSource: () -> Unicode.Scalar?
  ) -> ScalarAndCompatNormData? {

    switch state {
    case .emittingSegment:
      _internalInvariant(
        pendingStarter != nil,
        "Must find next segment starter before emitting buffered segment"
      )
      _internalInvariant(
        bufferIsSorted,
        "Buffered segment must be sorted before being emitted"
      )

      if let buffered = buffer.next() {
        return buffered
      }
      bufferIsSorted = false
      state = .consuming
      fallthrough

    case .consuming:

      while let (scalar, normData) = takePendingOrConsume(nextFromSource) {

        // If this scalar is a starter, stash it and emit the decomposed segment
        // we have in the buffer. The buffer must be sorted first.

        if normData.canonicalCombiningClass == .notReordered, !buffer.isEmpty {
          pendingStarter = (scalar, normData)
          buffer.sort()
          bufferIsSorted = true
          state = .emittingSegment
          return buffer.next()
        }

        // If this scalar is NFKD_QC, it does not need to be decomposed.

        if normData.isNFKDQC {

          // If the scalar is a starter its CCC is 0,
          // so it does not need to be sorted and can be emitted directly.

          if normData.canonicalCombiningClass == .notReordered {
            return (scalar, normData)
          }
          buffer.append((scalar, normData))
          continue
        }

        // Otherwise, append the scalar's decomposition to the buffer.

        decomposeNonNFKDQC((scalar, normData))
      }

      // Source is exhausted.
      return nil

    case .terminated:
      return nil
    }
  }

  internal mutating func _flush() -> ScalarAndCompatNormData? {

    state = .terminated

    if !bufferIsSorted {
      buffer.sort()
      bufferIsSorted = true
    }

    // The buffer contains the decomposed segment *prior to*
    // any pending starter we might have.

    return buffer.next() ?? pendingStarter.take()
  }

  @inline(__always)
  private mutating func takePendingOrConsume(
    _ nextFromSource: () -> Unicode.Scalar?
  ) -> ScalarAndCompatNormData? {

    if let pendingStarter = pendingStarter.take() {
      return pendingStarter
    } else if let nextScalar = nextFromSource() {
      return (nextScalar, Unicode._CompatibilityNormData(nextScalar))
    } else {
      return nil
    }
  }

  private mutating func decomposeNonNFKDQC(
    _ scalarInfo: ScalarAndCompatNormData
  ) {
    // Handle Hangul decomposition algorithmically.
    // S.base = 0xAC00
    // S.count = 11172
    // S.base + S.count - 1 = 0xD7A3
    if (0xAC00 ... 0xD7A3).contains(scalarInfo.scalar.value) {
      decomposeHangul(scalarInfo.scalar)
      return
    }

    // Otherwise, we need to lookup the decomposition (if there is one).
    decomposeSlow(scalarInfo)
  }

  @inline(never)
  private mutating func decomposeHangul(_ scalar: Unicode.Scalar) {
    // L = Hangul leading consonants
    let L: (base: UInt32, count: UInt32) = (base: 0x1100, count: 19)
    // V = Hangul vowels
    let V: (base: UInt32, count: UInt32) = (base: 0x1161, count: 21)
    // T = Hangul tail consonants
    let T: (base: UInt32, count: UInt32) = (base: 0x11A7, count: 28)
    // N = Number of precomposed Hangul syllables that start with the same
    //     leading consonant. (There is no base for N).
    let N: (base: UInt32, count: UInt32) = (base: 0x0, count: 588)
    // S = Hangul precomposed syllables
    let S: (base: UInt32, count: UInt32) = (base: 0xAC00, count: 11172)

    let sIdx = scalar.value &- S.base

    let lIdx = sIdx / N.count
    let l = Unicode.Scalar(_value: L.base &+ lIdx)
    buffer.append((scalar: l, normData: .hangulLeadingConsonants))

    let vIdx = (sIdx % N.count) / T.count
    let v = Unicode.Scalar(_value: V.base &+ vIdx)
    buffer.append((scalar: v, normData: .hangulVowels))

    let tIdx = sIdx % T.count
    if tIdx != 0 {
      let t = Unicode.Scalar(_value: T.base &+ tIdx)
      buffer.append((scalar: t, normData: .hangulTailConsonants))
    }
  }

  @inline(never)
  private mutating func decomposeSlow(
    _ original: ScalarAndCompatNormData
  ) {

    guard let decomp = Unicode._CompatibilityDecomposition(original.scalar) else {
      buffer.append(original)
      return
    }

    var utf8 = decomp.utf8
    while utf8.count > 0 {
      let (scalar, len) = _decodeScalar(utf8, startingAt: 0)
      utf8 = UnsafeBufferPointer(rebasing: utf8[len...])

      // FIXME: This could be a _CanonicalNormData lookup.
      // We'd only need CCC for sorting, and NFC_QC is useful for composition.
      let normData = Unicode._CompatibilityNormData(scalar)
      buffer.append((scalar, normData))
    }
  }
}
