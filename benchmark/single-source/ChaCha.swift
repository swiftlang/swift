//===--- ChaCha.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014-2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

/// This benchmark tests two things:
///
/// 1. Swift's ability to optimise low-level bit twiddling code.
/// 2. Swift's ability to optimise generic code when using contiguous data structures.
///
/// In principle initializing ChaCha20's state and then xoring the keystream with the
/// plaintext should be able to be vectorised.
enum ChaCha20 { }

extension ChaCha20 {
    public static func encrypt<Key: Collection, Nonce: Collection, Bytes: MutableCollection>(bytes: inout Bytes, key: Key, nonce: Nonce, initialCounter: UInt32 = 0) where Bytes.Element == UInt8, Key.Element == UInt8, Nonce.Element == UInt8 {
        var baseState = ChaChaState(key: key, nonce: nonce, counter: initialCounter)
        var index = bytes.startIndex

        while index < bytes.endIndex {
            let keyStream = baseState.block()
            keyStream.xorBytes(bytes: &bytes, at: &index)
            baseState.incrementCounter()
        }
    }
}


typealias BackingState = (UInt32, UInt32, UInt32, UInt32,
                          UInt32, UInt32, UInt32, UInt32,
                          UInt32, UInt32, UInt32, UInt32,
                          UInt32, UInt32, UInt32, UInt32)

struct ChaChaState {
    /// The ChaCha20 algorithm has 16 32-bit integer numbers as its state.
    /// They are traditionally laid out as a matrix: we do the same.
    var _state: BackingState

    /// Create a ChaChaState.
    ///
    /// The inputs to ChaCha20 are:
    ///
    /// - A 256-bit key, treated as a concatenation of eight 32-bit little-
    ///    endian integers.
    /// - A 96-bit nonce, treated as a concatenation of three 32-bit little-
    ///    endian integers.
    /// - A 32-bit block count parameter, treated as a 32-bit little-endian
    ///    integer.
    init<Key: Collection, Nonce: Collection>(key: Key, nonce: Nonce, counter: UInt32) where Key.Element == UInt8, Nonce.Element == UInt8 {
        guard key.count == 32 && nonce.count == 12 else {
            fatalError("Invalid key or nonce length.")
        }

        // The ChaCha20 state is initialized as follows:
        //
        // - The first four words (0-3) are constants: 0x61707865, 0x3320646e,
        //     0x79622d32, 0x6b206574.
        self._state.0 = 0x61707865
        self._state.1 = 0x3320646e
        self._state.2 = 0x79622d32
        self._state.3 = 0x6b206574

        // - The next eight words (4-11) are taken from the 256-bit key by
        //     reading the bytes in little-endian order, in 4-byte chunks.
        //
        // We force unwrap here because we have already preconditioned on the length.
        var keyIterator = CollectionOf32BitLittleEndianIntegers(key).makeIterator()
        self._state.4 = keyIterator.next()!
        self._state.5 = keyIterator.next()!
        self._state.6 = keyIterator.next()!
        self._state.7 = keyIterator.next()!
        self._state.8 = keyIterator.next()!
        self._state.9 = keyIterator.next()!
        self._state.10 = keyIterator.next()!
        self._state.11 = keyIterator.next()!


        // - Word 12 is a block counter.  Since each block is 64-byte, a 32-bit
        //     word is enough for 256 gigabytes of data.
        self._state.12 = counter

        // - Words 13-15 are a nonce, which should not be repeated for the same
        //     key.  The 13th word is the first 32 bits of the input nonce taken
        //     as a little-endian integer, while the 15th word is the last 32
        //     bits.
        //
        // Again, we forcibly unwrap these bytes.
        var nonceIterator = CollectionOf32BitLittleEndianIntegers(nonce).makeIterator()
        self._state.13 = nonceIterator.next()!
        self._state.14 = nonceIterator.next()!
        self._state.15 = nonceIterator.next()!
    }

    /// As a performance enhancement, it is often useful to be able to increment the counter portion directly. This avoids the
    /// expensive construction cost of the ChaCha state for each next sequence of bytes of the keystream.
    mutating func incrementCounter() {
        self._state.12 &+= 1
    }


    private mutating func add(_ otherState: ChaChaState) {
        self._state.0 &+= otherState._state.0
        self._state.1 &+= otherState._state.1
        self._state.2 &+= otherState._state.2
        self._state.3 &+= otherState._state.3
        self._state.4 &+= otherState._state.4
        self._state.5 &+= otherState._state.5
        self._state.6 &+= otherState._state.6
        self._state.7 &+= otherState._state.7
        self._state.8 &+= otherState._state.8
        self._state.9 &+= otherState._state.9
        self._state.10 &+= otherState._state.10
        self._state.11 &+= otherState._state.11
        self._state.12 &+= otherState._state.12
        self._state.13 &+= otherState._state.13
        self._state.14 &+= otherState._state.14
        self._state.15 &+= otherState._state.15
    }

    private mutating func columnRound() {
        // The column round:
        //
        // 1.  QUARTERROUND ( 0, 4, 8,12)
        // 2.  QUARTERROUND ( 1, 5, 9,13)
        // 3.  QUARTERROUND ( 2, 6,10,14)
        // 4.  QUARTERROUND ( 3, 7,11,15)
        ChaChaState.quarterRound(a: &self._state.0, b: &self._state.4, c: &self._state.8, d: &self._state.12)
        ChaChaState.quarterRound(a: &self._state.1, b: &self._state.5, c: &self._state.9, d: &self._state.13)
        ChaChaState.quarterRound(a: &self._state.2, b: &self._state.6, c: &self._state.10, d: &self._state.14)
        ChaChaState.quarterRound(a: &self._state.3, b: &self._state.7, c: &self._state.11, d: &self._state.15)
    }

    private mutating func diagonalRound() {
        // The diagonal round:
        //
        // 5.  QUARTERROUND ( 0, 5,10,15)
        // 6.  QUARTERROUND ( 1, 6,11,12)
        // 7.  QUARTERROUND ( 2, 7, 8,13)
        // 8.  QUARTERROUND ( 3, 4, 9,14)
        ChaChaState.quarterRound(a: &self._state.0, b: &self._state.5, c: &self._state.10, d: &self._state.15)
        ChaChaState.quarterRound(a: &self._state.1, b: &self._state.6, c: &self._state.11, d: &self._state.12)
        ChaChaState.quarterRound(a: &self._state.2, b: &self._state.7, c: &self._state.8, d: &self._state.13)
        ChaChaState.quarterRound(a: &self._state.3, b: &self._state.4, c: &self._state.9, d: &self._state.14)
    }
}

extension ChaChaState {
    static func quarterRound(a: inout UInt32, b: inout UInt32, c: inout UInt32, d: inout UInt32) {
        // The ChaCha quarter round. This is almost identical to the definition from RFC 7539
        // except that we use &+= instead of += because overflow modulo 32 is expected.
        a &+= b; d ^= a; d <<<= 16
        c &+= d; b ^= c; b <<<= 12
        a &+= b; d ^= a; d <<<= 8
        c &+= d; b ^= c; b <<<= 7
    }
}

extension ChaChaState {
    func block() -> ChaChaKeystreamBlock {
        var stateCopy = self  // We need this copy. This is cheaper than initializing twice.

        // The ChaCha20 block runs 10 double rounds (a total of 20 rounds), made of one column and
        // one diagonal round.
        for _ in 0..<10 {
            stateCopy.columnRound()
            stateCopy.diagonalRound()
        }

        // We add the original input words to the output words.
        stateCopy.add(self)

        return ChaChaKeystreamBlock(stateCopy)
    }
}


/// The result of running the ChaCha block function on a given set of ChaCha state.
///
/// This result has a distinct set of behaviours compared to the ChaChaState object, so we give it a different
/// (and more constrained) type.
struct ChaChaKeystreamBlock {
    var _state: BackingState

    init(_ state: ChaChaState) {
        self._state = state._state
    }

    /// A nice thing we can do with a ChaCha keystream block is xor it with some bytes.
    ///
    /// This helper function exists because we want a hook to do fast, in-place encryption of bytes.
    func xorBytes<Bytes: MutableCollection>(bytes: inout Bytes, at index: inout Bytes.Index) where Bytes.Element == UInt8 {
        // This is a naive implementation of this loop but I'm interested in testing the Swift compiler's ability
        // to optimise this. If we have a programmatic way to roll up this loop I'd love to hear it!
        self._state.0.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.1.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.2.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.3.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.4.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.5.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.6.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.7.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.8.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.9.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.10.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.11.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.12.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.13.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.14.xorLittleEndianBytes(bytes: &bytes, at: &index)
        if index == bytes.endIndex { return }
        self._state.15.xorLittleEndianBytes(bytes: &bytes, at: &index)
    }
}


infix operator <<<: BitwiseShiftPrecedence

infix operator <<<=: AssignmentPrecedence


extension FixedWidthInteger {
    func leftRotate(_ distance: Int) -> Self {
        return (self << distance) | (self >> (Self.bitWidth - distance))
    }

    mutating func rotatedLeft(_ distance: Int) {
        self = self.leftRotate(distance)
    }

    static func <<<(lhs: Self, rhs: Int) -> Self {
        return lhs.leftRotate(rhs)
    }

    static func <<<=(lhs: inout Self, rhs: Int) {
        lhs.rotatedLeft(rhs)
    }
}


struct CollectionOf32BitLittleEndianIntegers<BaseCollection: Collection> where BaseCollection.Element == UInt8 {
    var baseCollection: BaseCollection

    init(_ baseCollection: BaseCollection) {
        precondition(baseCollection.count % 4 == 0)
        self.baseCollection = baseCollection
    }
}

extension CollectionOf32BitLittleEndianIntegers: Collection {
    typealias Element = UInt32

    struct Index {
        var baseIndex: BaseCollection.Index

        init(_ baseIndex: BaseCollection.Index) {
            self.baseIndex = baseIndex
        }
    }

    var startIndex: Index {
        return Index(self.baseCollection.startIndex)
    }

    var endIndex: Index {
        return Index(self.baseCollection.endIndex)
    }

    func index(after index: Index) -> Index {
        return Index(self.baseCollection.index(index.baseIndex, offsetBy: 4))
    }

    subscript(_ index: Index) -> UInt32 {
        var baseIndex = index.baseIndex
        var result = UInt32(0)

        for shift in stride(from: 0, through: 24, by: 8) {
            result |= UInt32(self.baseCollection[baseIndex]) << shift
            self.baseCollection.formIndex(after: &baseIndex)
        }

        return result
    }
}

extension CollectionOf32BitLittleEndianIntegers.Index: Equatable {
    static func ==(lhs: Self, rhs: Self) -> Bool {
        return lhs.baseIndex == rhs.baseIndex
    }
}

extension CollectionOf32BitLittleEndianIntegers.Index: Comparable {
    static func <(lhs: Self, rhs: Self) -> Bool {
        return lhs.baseIndex < rhs.baseIndex
    }

    static func <=(lhs: Self, rhs: Self) -> Bool {
        return lhs.baseIndex <= rhs.baseIndex
    }

    static func >(lhs: Self, rhs: Self) -> Bool {
        return lhs.baseIndex > rhs.baseIndex
    }

    static func >=(lhs: Self, rhs: Self) -> Bool {
        return lhs.baseIndex >= rhs.baseIndex
    }
}


extension UInt32 {
    /// Performs an xor operation on up to 4 bytes of the mutable collection.
    func xorLittleEndianBytes<Bytes: MutableCollection>(bytes: inout Bytes, at index: inout Bytes.Index) where Bytes.Element == UInt8 {
        var loopCount = 0
        while index < bytes.endIndex && loopCount < 4  {
            bytes[index] ^= UInt8((self >> (loopCount * 8)) & UInt32(0xFF))
            bytes.formIndex(after: &index)
            loopCount += 1
        }
    }
}


public let ChaCha = BenchmarkInfo(
  name: "ChaCha",
  runFunction: run_ChaCha,
  tags: [.runtime, .cpubench])


@inline(never)
public func run_ChaCha(_ N: Int) {
  var plaintext = Array(repeating: UInt8(0), count: 30720)  // Chosen for CI runtime
  let key = Array(repeating: UInt8(1), count: 32)
  let nonce = Array(repeating: UInt8(2), count: 12)

  for _ in 1...N {
    ChaCha20.encrypt(bytes: &plaintext, key: key, nonce: nonce)
    blackHole(plaintext.first!)
  }
}
