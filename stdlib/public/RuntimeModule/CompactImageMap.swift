//===--- CompactImageMap.swift -------------------------------*- swift -*-===//
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
//  Definitions for Compact ImageMap Format
//
//===----------------------------------------------------------------------===//

import Swift

private let slash = UInt8(ascii: "/")
private let backslash = UInt8(ascii: "\\")

@_spi(Internal)
public enum CompactImageMapFormat {

  /// The list of fixed prefixes used to encode paths.
  static let fixedPathPrefixes = [
    // Traditional UNIX
    (0, "/lib"),
    (1, "/usr/lib"),
    (2, "/usr/local/lib"),
    (3, "/opt/lib"),

    // NeXT/Darwin
    (4, "/System/Library/Frameworks"),
    (5, "/System/Library/PrivateFrameworks"),
    (6, "/System/iOSSupport"),
    (7, "/Library/Frameworks"),
    (8, "/System/Applications"),
    (9, "/Applications"),

    // Windows
    (10, "C:\\Windows\\System32"),
    (11, "C:\\Program Files")
  ]

  /// Tells us what size of machine words were used when generating the
  /// image map.
  enum WordSize: UInt8 {
    case sixteenBit = 0
    case thirtyTwoBit = 1
    case sixtyFourBit = 2
  }

  /// Run a closure for each prefix of the specified string
  static func forEachPrefix(of str: String.UTF8View.SubSequence,
                            body: (String) -> ()) {
    let base = str.startIndex
    let end = str.endIndex
    var pos = base

    while pos < end {
      let ch = str[pos]

      if pos > base && (ch == slash || ch == backslash) {
        let range = base..<pos
        let prefix = String(str[range])!
        body(prefix)
      }

      pos = str.index(after: pos)
    }
  }

  /// Decodes a Sequence containing Compact ImageMap Format data into am
  /// ImageMap.
  @_spi(Internal)
  public struct Decoder<S: Sequence<UInt8>> {
    var sequence: S
    var iterator: S.Iterator
    var imageCount: Int = 0
    var wordSize: WordSize = .sixtyFourBit
    var wordMask: UInt64 = 0
    var pathPrefixes = Dictionary(uniqueKeysWithValues: fixedPathPrefixes)
    var nextCode = 32

    public init(_ sequence: S) {
      self.sequence = sequence
      self.iterator = sequence.makeIterator()
    }

    mutating func decodeCount() -> Int? {
      var value: Int = 0
      while true {
        guard let byte = iterator.next() else {
          return nil
        }

        value = (value << 7) | Int(byte & 0x7f)

        if (byte & 0x80) == 0 {
          break
        }
      }
      return value
    }

    mutating func decodeString() -> String? {
      guard let utf8Length = iterator.next() else {
        return nil
      }

      var bytes: [UInt8] = []
      bytes.reserveCapacity(Int(utf8Length))

      for _ in 0..<utf8Length {
        guard let byte = iterator.next() else {
          return nil
        }

        bytes.append(byte)
      }

      return String(decoding: bytes, as: UTF8.self)
    }

    mutating func decodeAddress(_ count: Int) -> UInt64? {
      var word: UInt64
      guard let firstByte = iterator.next() else {
        return nil
      }

      // Sign extend
      if (firstByte & 0x80) != 0 {
        word = wordMask | UInt64(firstByte)
      } else {
        word = UInt64(firstByte)
      }

      for _ in 1..<count {
        guard let byte = iterator.next() else {
          return nil
        }
        word = (word << 8) | UInt64(byte)
      }

      return word
    }

    mutating func decodePath() -> String? {
      var byte: UInt8

      guard let b = iterator.next() else {
        return nil
      }

      byte = b

      // `end` here means no string at all
      if byte == 0x00 {
        return nil
      }

      var resultBytes: [UInt8] = []
      var stringBase: Int? = nil

      while true {
        if byte == 0x00 {
          // `end`
          #if DEBUG_COMPACT_IMAGE_MAP
          print("end")
          #endif
          return String(decoding: resultBytes, as: UTF8.self)
        } else if byte < 0x40 {
          // `str`
          let count = Int(byte)
          resultBytes.reserveCapacity(resultBytes.count + count)
          let base = resultBytes.count
          if stringBase == nil {
            stringBase = base
          }
          for n in 0..<count {
            guard let char = iterator.next() else {
              return nil
            }
            if base + n > stringBase! && (char == slash
                                            || char == backslash) {
              let prefix = String(decoding: resultBytes[stringBase!..<base+n],
                                  as: UTF8.self)
              #if DEBUG_COMPACT_IMAGE_MAP
              print("define \(nextCode) = \(prefix)")
              #endif
              pathPrefixes[nextCode] = prefix
              nextCode += 1
            }
            resultBytes.append(char)

            #if DEBUG_COMPACT_IMAGE_MAP
            var hex = String(char, radix: 16)
            if hex.count == 1 {
              hex = "0" + hex
            }
            #endif
          }

          #if DEBUG_COMPACT_IMAGE_MAP
          let theString = String(decoding: resultBytes[base...], as: UTF8.self)
          print("str '\(theString)'")
          #endif
        } else if byte < 0x80 {
          // `framewk`
          let count = Int((byte & 0x3f) + 1)

          guard let version = iterator.next() else {
            return nil
          }

          var nameBytes: [UInt8] = []
          nameBytes.reserveCapacity(count)

          for _ in 0..<count {
            guard let char = iterator.next() else {
              return nil
            }
            nameBytes.append(char)
          }

          #if DEBUG_COMPACT_IMAGE_MAP
          let name = String(decoding: nameBytes, as: UTF8.self)
          let versionChar = String(Unicode.Scalar(version))
          print("framewk version='\(versionChar)' name='\(name)'")
          #endif

          resultBytes.append(slash)
          resultBytes.append(contentsOf: nameBytes)
          resultBytes.append(contentsOf: ".framework/Versions/".utf8)
          resultBytes.append(version)
          resultBytes.append(slash)
          resultBytes.append(contentsOf: nameBytes)

          return String(decoding: resultBytes, as: UTF8.self)
        } else {
          // `expand`
          var code: Int
          if (byte & 0x40) == 0 {
            code = Int(byte & 0x3f)
          } else {
            let byteCount = Int(byte & 0x3f) + 1
            code = 0
            for _ in 0..<byteCount {
              guard let byte = iterator.next() else {
                return nil
              }
              code = (code << 8) | Int(byte)
            }
            code += 64
          }

          #if DEBUG_COMPACT_IMAGE_MAP
          print("expand \(code) = \(String(describing: pathPrefixes[code]))")
          #endif

          guard let prefix = pathPrefixes[code] else {
            return nil
          }

          resultBytes.append(contentsOf: prefix.utf8)
        }

        guard let b = iterator.next() else {
          return nil
        }

        byte = b
      }
    }

    mutating func decode() -> (String, [ImageMap.Image], ImageMap.WordSize)? {
      // Check the version and decode the size
      guard let infoByte = iterator.next() else {
        return nil
      }
      let version = infoByte >> 2
      guard let size = WordSize(rawValue: infoByte & 0x3) else {
        return nil
      }
      wordSize = size
      guard version == 0 else {
        return nil
      }

      // Set up the word mask
      switch wordSize {
        case .sixteenBit:
          wordMask = 0xff00
        case .thirtyTwoBit:
          wordMask = 0xffffff00
        case .sixtyFourBit:
          wordMask = 0xffffffffffffff00
      }

      // Now decode the platform
      guard let platform = decodeString() else {
        return nil
      }

      // Next is the image count
      guard let count = decodeCount() else {
        return nil
      }

      imageCount = count

      // Now decode all of the images
      var images: [ImageMap.Image] = []
      var lastAddress: UInt64 = 0

      images.reserveCapacity(count)

      for _ in 0..<count {
        // Decode the header byte
        guard let header = iterator.next() else {
          return nil
        }

        let relative = (header & 0x80) != 0
        let acount = Int(((header >> 3) & 0x7) + 1)
        let ecount = Int((header & 0x7) + 1)

        #if DEBUG_COMPACT_IMAGE_MAP
        print("r = \(relative), acount = \(acount), ecount = \(ecount)")
        #endif

        // Now the base and end of text addresses
        guard let address = decodeAddress(acount) else {
          return nil
        }
        let baseAddress: UInt64
        if relative {
          baseAddress = lastAddress &+ address
        } else {
          baseAddress = address
        }

        lastAddress = baseAddress

        guard let eotOffset = decodeAddress(ecount) else {
          return nil
        }
        let endOfText = baseAddress &+ eotOffset

        #if DEBUG_COMPACT_IMAGE_MAP
        print("address = \(hex(address)), eotOffset = \(hex(eotOffset))")
        print("baseAddress = \(hex(baseAddress)), endOfText = \(hex(endOfText))")
        #endif

        // Next, get the build ID byte count
        guard let buildIdBytes = decodeCount() else {
          return nil
        }

        #if DEBUG_COMPACT_IMAGE_MAP
        print("buildIdBytes = \(buildIdBytes)")
        #endif

        // Read the build ID
        var buildId: [UInt8]? = nil

        if buildIdBytes > 0 {
          buildId = []
          buildId!.reserveCapacity(buildIdBytes)

          for _ in 0..<buildIdBytes {
            guard let byte = iterator.next() else {
              return nil
            }
            buildId!.append(byte)
          }
        }

        #if DEBUG_COMPACT_IMAGE_MAP
        print("buildId = \(buildId)")
        #endif

        // Decode the path
        let path = decodePath()
        let name: String?

        // Extract the name from the path
        if let path = path {
          if let lastSlashNdx = path.utf8.lastIndex(
               where: { $0 == slash || $0 == backslash }
             ) {
            let nameNdx = path.index(after: lastSlashNdx)

            name = String(path[nameNdx...])
          } else {
            name = path
          }
        } else {
          name = nil
        }

        let image = ImageMap.Image(
          name: name,
          path: path,
          uniqueID: buildId,
          baseAddress: baseAddress,
          endOfText: endOfText
        )

        images.append(image)
      }

      let wsMap: ImageMap.WordSize
      switch wordSize {
        case .sixteenBit:
          wsMap = .sixteenBit
        case .thirtyTwoBit:
          wsMap = .thirtyTwoBit
        case .sixtyFourBit:
          wsMap = .sixtyFourBit
      }

      return (platform, images, wsMap)
    }
  }

  /// Encodes an ImageMap as a Sequence<UInt8>
  @_spi(Internal)
  public struct Encoder: Sequence {
    public typealias Element = UInt8

    private var source: ImageMap

    public init(_ source: ImageMap) {
      self.source = source
    }

    public func makeIterator() -> Iterator {
      return Iterator(source)
    }

    public struct Iterator: IteratorProtocol {
      enum State {
        case start
        case platform(Int)
        case count(Int)
        case image
        case baseAddress(Int)
        case endOfText(Int)
        case uniqueID(Int)
        case uniqueIDBytes(Int)
        case path
        case pathCode(Int)
        case pathString
        case pathStringChunk(Int)
        case version
        case framework
        case done
      }

      var abytes = EightByteBuffer()
      var ebytes = EightByteBuffer()
      var acount: Int = 0
      var ecount: Int = 0
      var version: UInt8 = 0
      var lastAddress: UInt64 = 0
      var ndx: Int = 0
      var state: State = .start
      var source: ImageMap
      var pathPrefixes = fixedPathPrefixes
      var nextCode = 32
      var remainingPath: String.UTF8View.SubSequence?

      func signExtend(_ value: UInt64) -> UInt64 {
        let mask: UInt64
        let topBit: UInt64
        switch source.wordSize {
          case .sixteenBit:
            topBit = 0x8000
            mask = 0xffffffffffff0000
          case .thirtyTwoBit:
            topBit = 0x80000000
            mask = 0xffffffff00000000
          case .sixtyFourBit:
            return value
        }

        if (value & topBit) != 0 {
          return value | mask
        }
        return value
      }

      init(_ source: ImageMap) {
        self.source = source
      }

      public mutating func next() -> UInt8? {
        switch state {
          case .done:
            return nil

          case .start:
            // The first thing we emit is the info byte
            let size: WordSize
            switch source.wordSize {
              case .sixteenBit:
                size = .sixteenBit
              case .thirtyTwoBit:
                size = .thirtyTwoBit
              case .sixtyFourBit:
                size = .sixtyFourBit
            }

            state = .platform(-1)

            let version: UInt8 = 0
            let infoByte = (version << 2) | size.rawValue
            return infoByte

          case let .platform(ndx):
            let length = UInt8(source.platform.utf8.count)
            let byte: UInt8

            if ndx == -1 {
              // The length byte comes first
              byte = length
            } else {
              byte = source.platform.utf8[
                source.platform.utf8.index(
                  source.platform.utf8.startIndex,
                  offsetBy: ndx
                )
              ]
            }

            // If we're done, move to the .count state
            if ndx + 1 == length {
              let count = source.images.count
              let bits = Int.bitWidth - count.leadingZeroBitCount
              state = .count(7 * (bits / 7))
            } else {
              state = .platform(ndx + 1)
            }

            return byte

          case let .count(ndx):
            let count = source.images.count
            let byte = UInt8(truncatingIfNeeded:(count >> ndx) & 0x7f)
            if ndx == 0 {
              state = .image
              return byte
            } else {
              state = .count(ndx - 7)
              return 0x80 | byte
            }

          case .image:
            if ndx == source.images.count {
              state = .done
              return nil
            }

            let baseAddress = signExtend(source.images[ndx].baseAddress)
            let delta = baseAddress &- lastAddress

            let endOfText = signExtend(source.images[ndx].endOfText)
            let endOfTextOffset = endOfText - baseAddress

            let eotCount: Int
            if endOfTextOffset & (1 << 63) != 0 {
              let ones = ((~endOfTextOffset).leadingZeroBitCount - 1) >> 3
              eotCount = 8 - ones
            } else {
              let zeroes = (endOfTextOffset.leadingZeroBitCount - 1) >> 3
              eotCount = 8 - zeroes
            }

            ebytes = EightByteBuffer(endOfTextOffset)
            ecount = eotCount

            let absCount: Int
            if baseAddress & (1 << 63) != 0 {
              let ones = ((~baseAddress).leadingZeroBitCount - 1) >> 3
              absCount = 8 - ones
            } else {
              let zeroes = (baseAddress.leadingZeroBitCount - 1) >> 3
              absCount = 8 - zeroes
            }

            let deltaCount: Int
            if delta & (1 << 63) != 0 {
              let ones = ((~delta).leadingZeroBitCount - 1) >> 3
              deltaCount = 8 - ones
            } else {
              let zeroes = (delta.leadingZeroBitCount - 1) >> 3
              deltaCount = 8 - zeroes
            }

            lastAddress = baseAddress

            let relativeFlag: UInt8
            if absCount <= deltaCount {
              abytes = EightByteBuffer(baseAddress)
              acount = absCount
              relativeFlag = 0
            } else {
              abytes = EightByteBuffer(delta)
              acount = deltaCount
              relativeFlag = 0x80
            }

            state = .baseAddress(8 - acount)
            return relativeFlag
              | UInt8(truncatingIfNeeded: (acount - 1) << 3)
              | UInt8(truncatingIfNeeded: ecount - 1)

          case let .baseAddress(ndx):
            let byte = abytes[ndx]
            if ndx + 1 == 8 {
              state = .endOfText(8 - ecount)
            } else {
              state = .baseAddress(ndx + 1)
            }
            return byte

          case let .endOfText(ndx):
            let byte = ebytes[ndx]
            if ndx + 1 == 8 {
              let count = source.images[self.ndx].uniqueID?.count ?? 0
              let bits = Int.bitWidth - count.leadingZeroBitCount
              state = .uniqueID(7 * (bits / 7))
            } else {
              state = .endOfText(ndx + 1)
            }
            return byte

          case let .uniqueID(cndx):
            guard let count = source.images[self.ndx].uniqueID?.count else {
              state = .path
              if let path = source.images[self.ndx].path {
                remainingPath = path.utf8[...]
              } else {
                remainingPath = nil
              }
              return 0
            }
            let byte = UInt8(truncatingIfNeeded: (count >> cndx) & 0x7f)
            if cndx == 0 {
              state = .uniqueIDBytes(0)
              return byte
            } else {
              state = .uniqueID(cndx - 7)
              return 0x80 | byte
            }

          case let .uniqueIDBytes(byteNdx):
            let uniqueID = source.images[self.ndx].uniqueID!
            let byte = uniqueID[byteNdx]
            if byteNdx + 1 == uniqueID.count {
              state = .path
              if let path = source.images[self.ndx].path {
                remainingPath = path.utf8[...]
              } else {
                remainingPath = nil
              }
            } else {
              state = .uniqueIDBytes(byteNdx + 1)
            }
            return byte

          case .path:
            guard let remainingPath = remainingPath,
                  remainingPath.count > 0 else {
              ndx += 1
              state = .image
              return 0x00
            }

            // Find the longest prefix match
            var longestMatchLen = 0
            var matchedPrefix: Int? = nil
            for (ndx, (_, prefix)) in pathPrefixes.enumerated() {
              let prefixUTF8 = prefix.utf8
              if prefixUTF8.count > remainingPath.count {
                continue
              }
              if prefixUTF8.count > longestMatchLen
                   && remainingPath.starts(with: prefixUTF8) {
                longestMatchLen = prefixUTF8.count
                matchedPrefix = ndx
              }
            }

            if let ndx = matchedPrefix {
              let (code, prefix) = pathPrefixes[ndx]
              self.remainingPath = remainingPath.dropFirst(prefix.utf8.count)
              if code <= 0x3f {
                return 0x80 | UInt8(exactly: code)!
              }

              let theCode = UInt64(exactly: code - 0x40)!
              abytes = EightByteBuffer(theCode)

              let codeBytes = Swift.max(
                (64 - theCode.leadingZeroBitCount) >> 3, 1
              )

              state = .pathCode(8 - codeBytes)

              return 0xc0 | UInt8(exactly: codeBytes - 1)!
            }

            // Check for /<name>.framework/Versions/<version>/<name>
            if let name = source.images[ndx].name, !name.isEmpty {
              let nameCount = name.utf8.count
              let expectedLen = 1 // '/'
                + nameCount       // <name>
                + 20              // .framework/Versions/
                + 1               // <version>
                + 1               // '/'
                + nameCount       // <name>
              if remainingPath.count == expectedLen {
                let framework = "/\(name).framework/Versions/"
                if remainingPath.starts(with: framework.utf8) {
                  var verNdx = remainingPath.startIndex
                  remainingPath.formIndex(&verNdx, offsetBy: framework.utf8.count)

                  version = remainingPath[verNdx]

                  let slashNdx = remainingPath.index(after: verNdx)
                  if remainingPath[slashNdx] == slash {
                    let nameNdx = remainingPath.index(after: slashNdx)
                    if remainingPath[nameNdx...].elementsEqual(name.utf8) {
                      self.remainingPath = remainingPath[nameNdx...]

                      state = .version
                      return 0x40 | UInt8(exactly: nameCount - 1)!
                    }
                  }
                }
              }
            }

            // Add any new prefixes
            forEachPrefix(of: remainingPath) { prefix in
              #if DEBUG_COMPACT_IMAGE_MAP
              print("defining \(nextCode) as \"\(prefix)\"")
              #endif
              pathPrefixes.append((nextCode, prefix))
              nextCode += 1
            }

            fallthrough

          case .pathString:
            if remainingPath!.count == 0 {
              ndx += 1
              state = .image
              return 0x00
            }

            let chunkLength = Swift.min(remainingPath!.count, 0x3f)
            state = .pathStringChunk(chunkLength)
            return UInt8(truncatingIfNeeded: chunkLength)

          case let .pathStringChunk(length):
            let byte = remainingPath!.first!
            remainingPath = remainingPath!.dropFirst()
            if length == 1 {
              state = .pathString
            } else {
              state = .pathStringChunk(length - 1)
            }
            return byte

          case .version:
            state = .framework
            return version

          case .framework:
            let byte = remainingPath!.first!
            remainingPath = remainingPath!.dropFirst()
            if remainingPath!.count == 0 {
              ndx += 1
              state = .image
            }
            return byte

          case let .pathCode(ndx):
            let byte = abytes[ndx]
            if ndx + 1 == 8 {
              state = .path
            } else {
              state = .pathCode(ndx + 1)
            }
            return byte
        }
      }
    }
  }

}
