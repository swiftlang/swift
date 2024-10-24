//===--- CompactBacktrace.swift -------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Definitions for Compact Backtrace Format
//
//===----------------------------------------------------------------------===//

import Swift

enum CompactBacktraceFormat {
  /// Tells us what size of machine words were used when generating the
  /// backtrace.
  enum WordSize: UInt8 {
    case sixteenBit = 0
    case thirtyTwoBit = 1
    case sixtyFourBit = 2
  }

  // Instruction encodings
  struct Instruction: RawRepresentable {
    typealias RawValue = UInt8

    private(set) var rawValue: UInt8

    init?(rawValue: Self.RawValue) {
      self.rawValue = rawValue
    }

    static let end         = Instruction(rawValue: 0b00000000)!
    static let trunc       = Instruction(rawValue: 0b00000001)!

    static let pc_first    = Instruction(rawValue: 0b00010000)!
    static let pc_last     = Instruction(rawValue: 0b00011111)!
    static let ra_first    = Instruction(rawValue: 0b00100000)!
    static let ra_last     = Instruction(rawValue: 0b00101111)!
    static let async_first = Instruction(rawValue: 0b00110000)!
    static let async_last  = Instruction(rawValue: 0b00111111)!

    static let omit_first  = Instruction(rawValue: 0b01000000)!
    static let omit_last   = Instruction(rawValue: 0b01111111)!

    private static func addressInstr(
      _ code: UInt8, _ absolute: Bool, _ count: Int
    ) -> Instruction {
      return Instruction(rawValue: code
                           | (absolute ? 0b00001000 : 0)
                           | UInt8(count - 1))!
    }

    static func pc(absolute: Bool, count: Int) -> Instruction {
      return addressInstr(0b00010000, absolute, count)
    }
    static func ra(absolute: Bool, count: Int) -> Instruction {
      return addressInstr(0b00100000, absolute, count)
    }
    static func `async`(absolute: Bool, count: Int) -> Instruction {
      return addressInstr(0b00110000, absolute, count)
    }

    static func omit(external: Bool, count: Int) -> Instruction {
      return Instruction(rawValue: 0b01000000
                           | (external ? 0b00100000 : 0)
                           | UInt8(count - 1))!
    }
  }

  // Represents a decoded instruction
  enum DecodedInstruction {
    case end
    case trunc
    case pc(absolute: Bool, count: Int)
    case ra(absolute: Bool, count: Int)
    case `async`(absolute: Bool, count: Int)
    case omit(external: Bool, count: Int)
  }


  /// Adapts a Sequence containing Compact Backtrace Format data into a
  /// Sequence of `Backtrace.Frame`s.
  struct Decoder<S: Sequence<UInt8>>: Sequence {
    typealias Frame = Backtrace.Frame
    typealias Address = Backtrace.Address
    typealias Storage = S

    private var storage: Storage

    init(_ storage: S) {
      self.storage = storage
    }

    public func makeIterator() -> Iterator {
      var iterator = storage.makeIterator()
      guard let infoByte = iterator.next() else {
        return Iterator(nil, .sixtyFourBit)
      }
      let version = infoByte >> 2
      guard let size = WordSize(rawValue: infoByte & 0x3) else {
        return Iterator(nil, .sixtyFourBit)
      }
      guard version == 0 else {
        return Iterator(nil, .sixtyFourBit)
      }
      return Iterator(iterator, size)
    }

    struct Iterator: IteratorProtocol {
      var iterator: Storage.Iterator?
      let wordSize: WordSize
      let wordMask: UInt64
      var lastAddress: UInt64

      init(_ iterator: Storage.Iterator?, _ size: WordSize) {
        self.iterator = iterator
        self.wordSize = size

        switch size {
          case .sixteenBit:
            self.wordMask = 0xff00
          case .thirtyTwoBit:
            self.wordMask = 0xffffff00
          case .sixtyFourBit:
            self.wordMask = 0xffffffffffffff00
        }

        self.lastAddress = 0
      }

      private mutating func decodeAddress(
        _ absolute: Bool, _ count: Int
      ) -> Address? {
        var word: UInt64
        guard let firstByte = iterator!.next() else {
          return nil
        }
        if (firstByte & 0x80) != 0 {
          word = wordMask | UInt64(firstByte)
        } else {
          word = UInt64(firstByte)
        }
        for _ in 1..<count {
          guard let byte = iterator!.next() else {
            return nil
          }
          word = (word << 8) | UInt64(byte)
        }

        if absolute {
          lastAddress = word
        } else {
          lastAddress = lastAddress &+ word
        }

        switch wordSize {
          case .sixteenBit:
            return Address(UInt16(truncatingIfNeeded: lastAddress))
          case .thirtyTwoBit:
            return Address(UInt32(truncatingIfNeeded: lastAddress))
          case .sixtyFourBit:
            return Address(UInt64(truncatingIfNeeded: lastAddress))
        }
      }

      private mutating func finished() {
        iterator = nil
      }

      private mutating func fail() {
        iterator = nil
      }

      // Note: If we hit an error while decoding, we will return .trucnated.

      public mutating func next() -> Frame? {
        if iterator == nil {
          return nil
        }

        guard let instr = iterator!.next() else {
          finished()
          return .truncated
        }

        guard let decoded = Instruction(rawValue: instr)?.decoded() else {
          fail()
          return .truncated
        }

        switch decoded {
          case .end:
            finished()
            return nil
          case .trunc:
            finished()
            return .truncated
          case let .pc(absolute, count):
            guard let addr = decodeAddress(absolute, count) else {
              finished()
              return .truncated
            }
            return .programCounter(addr)
          case let .ra(absolute, count):
            guard let addr = decodeAddress(absolute, count) else {
              finished()
              return .truncated
            }
            return .returnAddress(addr)
          case let .async(absolute, count):
            guard let addr = decodeAddress(absolute, count) else {
              finished()
              return .truncated
            }
            return .asyncResumePoint(addr)
          case let .omit(external, count):
            if !external {
              return .omittedFrames(count)
            } else {
              var word: Int = 0
              for _ in 0..<count {
                guard let byte = iterator!.next() else {
                  finished()
                  return .truncated
                }
                word = (word << 8) | Int(byte)
              }
              return .omittedFrames(word)
            }
        }
      }
    }

  }

  /// Adapts a Sequence of RichFrames into a sequence containing Compact
  /// Backtrace Format data.
  struct Encoder<A: FixedWidthInteger, S: Sequence<RichFrame<A>>>: Sequence {
    typealias Element = UInt8
    typealias Frame = Backtrace.Frame
    typealias Address = A
    typealias Source = S

    private var source: Source

    init(_ source: Source) {
      self.source = source
    }

    public func makeIterator() -> Iterator {
      return Iterator(source.makeIterator())
    }

    struct Iterator: IteratorProtocol {
      var iterator: Source.Iterator
      var lastAddress: Address = 0

      enum State {
        case start
        case ready
        case emittingBytes(Int)
        case done
      }
      var bytes = EightByteBuffer()
      var state: State = .start

      init(_ iterator: Source.Iterator) {
        self.iterator = iterator
      }

      /// Set up to emit the bytes of `address`, returning the number of bytes
      /// we will need to emit
      private mutating func emitAddressNext(
        _ address: Address
      ) -> (absolute: Bool, count: Int) {
        let delta = address &- lastAddress

        let absCount: Int
        if address & (1 << (Address.bitWidth - 1)) != 0 {
          let ones = ((~address).leadingZeroBitCount - 1) >> 3
          absCount = (Address.bitWidth >> 3) - ones
        } else {
          let zeroes = (address.leadingZeroBitCount - 1) >> 3
          absCount = (Address.bitWidth >> 3) - zeroes
        }

        let deltaCount: Int
        if delta & (1 << (Address.bitWidth - 1)) != 0 {
          let ones = ((~delta).leadingZeroBitCount - 1) >> 3
          deltaCount = (Address.bitWidth >> 3) - ones
        } else {
          let zeroes = (delta.leadingZeroBitCount - 1) >> 3
          deltaCount = (Address.bitWidth >> 3) - zeroes
        }

        lastAddress = address

        if absCount < deltaCount {
          bytes = EightByteBuffer(address)
          state = .emittingBytes(8 - absCount)
          return (absolute: true, count: absCount)
        } else {
          bytes = EightByteBuffer(delta)
          state = .emittingBytes(8 - deltaCount)
          return (absolute: false, count: deltaCount)
        }
      }

      /// Set up to emit the bytes of `count`, returning the number of bytes
      /// we will need to emit
      private mutating func emitExternalCountNext(
        _ count: Int
      ) -> Int {
        let ucount = UInt64(count)
        let zeroes = ucount.leadingZeroBitCount >> 3
        let byteCount = 8 - zeroes
        bytes = EightByteBuffer(ucount)
        state = .emittingBytes(zeroes)
        return byteCount
      }

      public mutating func next() -> UInt8? {
        switch state {
          case .done:
            return nil

          case .start:
            // The first thing we emit is the info byte
            let size: WordSize
            switch Address.bitWidth {
              case 16:
                size = .sixteenBit
              case 32:
                size = .thirtyTwoBit
              case 64:
                size = .sixtyFourBit
              default:
                state = .done
                return nil
            }

            state = .ready

            let version: UInt8 = 0
            let infoByte = (version << 2) | size.rawValue
            return infoByte

          case let .emittingBytes(ndx):

            let byte = bytes[ndx]
            if ndx + 1 == 8 {
              state = .ready
            } else {
              state = .emittingBytes(ndx + 1)
            }
            return byte

          case .ready:

            // Grab a rich frame and encode it
            guard let frame = iterator.next() else {
              state = .done
              return nil
            }

            switch frame {
              case let .programCounter(addr):
                let (absolute, count) = emitAddressNext(addr)
                return Instruction.pc(absolute: absolute,
                                      count: count).rawValue
              case let .returnAddress(addr):
                let (absolute, count) = emitAddressNext(addr)
                return Instruction.ra(absolute: absolute,
                                      count: count).rawValue
              case let .asyncResumePoint(addr):
                let (absolute, count) = emitAddressNext(addr)
                return Instruction.async(absolute: absolute,
                                         count: count).rawValue
              case let .omittedFrames(count):
                if count <= 0x1f {
                  return Instruction.omit(external: false,
                                          count: count).rawValue
                }
                let countCount = emitExternalCountNext(count)
                return Instruction.omit(external: true,
                                        count: countCount).rawValue
              case .truncated:
                self.state = .done
                return Instruction.trunc.rawValue
            }
        }
      }
    }
  }
}

extension CompactBacktraceFormat.Instruction: Comparable {
  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }
  public static func == (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }
}

extension CompactBacktraceFormat.Instruction {
  func decoded() -> CompactBacktraceFormat.DecodedInstruction? {
    switch self {
      case .end:
        return .end
      case .trunc:
        return .trunc
      case .pc_first ... .pc_last:
        let count = Int((self.rawValue & 0x7) + 1)
        let absolute = (self.rawValue & 0x8) != 0
        return .pc(absolute: absolute, count: count)
      case .ra_first ... .ra_last:
        let count = Int((self.rawValue & 0x7) + 1)
        let absolute = (self.rawValue & 0x8) != 0
        return .ra(absolute: absolute, count: count)
      case .async_first ... .async_last:
        let count = Int((self.rawValue & 0x7) + 1)
        let absolute = (self.rawValue & 0x8) != 0
        return .async(absolute: absolute, count: count)
      case .omit_first ... .omit_last:
        let count = Int((self.rawValue & 0x1f) + 1)
        let external = (self.rawValue & 0x20) != 0
        return .omit(external: external, count: count)
      default:
        return nil
    }
  }
}
