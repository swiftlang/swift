//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 9999, *)
extension AsyncSequence where Element == Unicode.Scalar {

  /// Normalized representations of this sequence's contents.
  ///
  @inlinable
  public var normalized: Unicode.NormalizedScalars<Self> {
    Unicode.NormalizedScalars(self)
  }
}

@available(SwiftStdlib 9999, *)
extension Unicode.NormalizedScalars
where Source: AsyncSequence, Source.Element == Unicode.Scalar {

  /// The contents of the source sequence, in Normalization Form D.
  ///
  /// Normalization to NFD preserves canonical equivalence.
  ///
  @inlinable
  public var nfd: AsyncNFD {
    AsyncNFD(source: source)
  }

  /// The contents of the source sequence, in Normalization Form D.
  ///
  /// Normalization to NFD preserves canonical equivalence.
  ///
  @frozen
  public struct AsyncNFD: AsyncSequence {

    public var source: Source

    @inlinable
    internal init(source: Source) {
      self.source = source
    }

    @inlinable
    public func makeAsyncIterator() -> AsyncIterator {
      AsyncIterator(source: source.makeAsyncIterator())
    }

    @frozen
    public struct AsyncIterator: AsyncIteratorProtocol {

      public typealias Element = Unicode.Scalar
      public typealias Failure = Source.Failure

      public var source: Source.AsyncIterator

      @usableFromInline
      internal var normalizer = Unicode.NFDNormalizer()
      @usableFromInline
      internal var pending = Optional<Unicode.Scalar>.none

      @inlinable
      internal init(source: Source.AsyncIterator) {
        self.source = source
      }

      @inlinable
      public mutating func next(
        isolation actor: isolated (any Actor)?
      ) async throws(Source.Failure) -> Unicode.Scalar? {

        // Equivalent to: "pending.take() ?? try await source.next()"
        func _pendingOrNextFromSource()
        async throws(Source.Failure) -> Unicode.Scalar? {
          if pending != nil { return pending.take() }
          return try await source.next(isolation: actor)
        }

        while let scalar = try await _pendingOrNextFromSource() {
          var iter = CollectionOfOne(scalar).makeIterator()
          if let output = normalizer.resume(consuming: &iter) {
            pending = iter.next()
            return output
          }
        }
        return normalizer.flush()
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension Unicode.NormalizedScalars
where Source: AsyncSequence, Source.Element == Unicode.Scalar {

  /// The contents of the source sequence, in Normalization Form C.
  ///
  /// Normalization to NFC preserves canonical equivalence.
  ///
  @inlinable
  public var nfc: AsyncNFC {
    AsyncNFC(source: source)
  }

  /// The contents of the source sequence, in Normalization Form C.
  ///
  /// Normalization to NFC preserves canonical equivalence.
  ///
  @frozen
  public struct AsyncNFC: AsyncSequence {

    public var source: Source

    @inlinable
    internal init(source: Source) {
      self.source = source
    }

    @inlinable
    public func makeAsyncIterator() -> AsyncIterator {
      AsyncIterator(source: source.makeAsyncIterator())
    }

    @frozen
    public struct AsyncIterator: AsyncIteratorProtocol {

      public typealias Element = Unicode.Scalar
      public typealias Failure = Source.Failure

      public var source: Source.AsyncIterator

      @usableFromInline
      internal var normalizer = Unicode.NFCNormalizer()
      @usableFromInline
      internal var pending = Optional<Unicode.Scalar>.none

      @inlinable
      internal init(source: Source.AsyncIterator) {
        self.source = source
      }

      @inlinable
      public mutating func next(
        isolation actor: isolated (any Actor)?
      ) async throws(Source.Failure) -> Unicode.Scalar? {

        // Equivalent to: "pending.take() ?? try await source.next()"
        func _pendingOrNextFromSource()
        async throws(Source.Failure) -> Unicode.Scalar? {
          if pending != nil { return pending.take() }
          return try await source.next(isolation: actor)
        }

        while let scalar = try await _pendingOrNextFromSource() {
          var iter = CollectionOfOne(scalar).makeIterator()
          if let output = normalizer.resume(consuming: &iter) {
            pending = iter.next()
            return output
          }
        }
        return normalizer.flush()
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension Unicode.NormalizedScalars
where Source: AsyncSequence, Source.Element == Unicode.Scalar {

  /// The contents of the source sequence, in Normalization Form KD.
  ///
  /// Normalization to NFKD does _not_ preserve canonical equivalence.
  ///
  @inlinable
  public var nfkd: AsyncNFKD {
    AsyncNFKD(source: source)
  }

  /// The contents of the source sequence, in Normalization Form KD.
  ///
  /// Normalization to NFKD does _not_ preserve canonical equivalence.
  ///
  @frozen
  public struct AsyncNFKD: AsyncSequence {

    public var source: Source

    @inlinable
    internal init(source: Source) {
      self.source = source
    }

    @inlinable
    public func makeAsyncIterator() -> AsyncIterator {
      AsyncIterator(source: source.makeAsyncIterator())
    }

    @frozen
    public struct AsyncIterator: AsyncIteratorProtocol {

      public typealias Element = Unicode.Scalar
      public typealias Failure = Source.Failure

      public var source: Source.AsyncIterator

      @usableFromInline
      internal var normalizer = Unicode.NFKDNormalizer()
      @usableFromInline
      internal var pending = Optional<Unicode.Scalar>.none

      @inlinable
      internal init(source: Source.AsyncIterator) {
        self.source = source
      }

      @inlinable
      public mutating func next(
        isolation actor: isolated (any Actor)?
      ) async throws(Source.Failure) -> Unicode.Scalar? {

        // Equivalent to: "pending.take() ?? try await source.next()"
        func _pendingOrNextFromSource()
        async throws(Source.Failure) -> Unicode.Scalar? {
          if pending != nil { return pending.take() }
          return try await source.next(isolation: actor)
        }

        while let scalar = try await _pendingOrNextFromSource() {
          var iter = CollectionOfOne(scalar).makeIterator()
          if let output = normalizer.resume(consuming: &iter) {
            pending = iter.next()
            return output
          }
        }
        return normalizer.flush()
      }
    }
  }
}

@available(SwiftStdlib 9999, *)
extension Unicode.NormalizedScalars
where Source: AsyncSequence, Source.Element == Unicode.Scalar {

  /// The contents of the source sequence, in Normalization Form KC.
  ///
  /// Normalization to NFKC does _not_ preserve canonical equivalence.
  ///
  @inlinable
  public var nfkc: AsyncNFKC {
    AsyncNFKC(source: source)
  }

  /// The contents of the source sequence, in Normalization Form KC.
  ///
  /// Normalization to NFKC does _not_ preserve canonical equivalence.
  ///
  @frozen
  public struct AsyncNFKC: AsyncSequence {

    public var source: Source

    @inlinable
    internal init(source: Source) {
      self.source = source
    }

    @inlinable
    public func makeAsyncIterator() -> AsyncIterator {
      AsyncIterator(source: source.makeAsyncIterator())
    }

    @frozen
    public struct AsyncIterator: AsyncIteratorProtocol {

      public typealias Element = Unicode.Scalar
      public typealias Failure = Source.Failure

      public var source: Source.AsyncIterator

      @usableFromInline
      internal var normalizer = Unicode.NFKCNormalizer()
      @usableFromInline
      internal var pending = Optional<Unicode.Scalar>.none

      @inlinable
      internal init(source: Source.AsyncIterator) {
        self.source = source
      }

      @inlinable
      public mutating func next(
        isolation actor: isolated (any Actor)?
      ) async throws(Source.Failure) -> Unicode.Scalar? {

        // Equivalent to: "pending.take() ?? try await source.next()"
        func _pendingOrNextFromSource()
        async throws(Source.Failure) -> Unicode.Scalar? {
          if pending != nil { return pending.take() }
          return try await source.next(isolation: actor)
        }

        while let scalar = try await _pendingOrNextFromSource() {
          var iter = CollectionOfOne(scalar).makeIterator()
          if let output = normalizer.resume(consuming: &iter) {
            pending = iter.next()
            return output
          }
        }
        return normalizer.flush()
      }
    }
  }
}
