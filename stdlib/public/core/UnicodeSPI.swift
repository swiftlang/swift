//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Unicode.NFD
//===----------------------------------------------------------------------===//

extension Unicode {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public struct _NFD {
    let base: Substring.UnicodeScalarView
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFD {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public struct Iterator {
    var base: Unicode._InternalNFD<Substring>.Iterator
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFD.Iterator: IteratorProtocol {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public mutating func next() -> Unicode.Scalar? {
    base.next()?.scalar
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFD: Sequence {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func makeIterator() -> Iterator {
    Iterator(base: Unicode._InternalNFD(base: base).makeIterator())
  }
}

extension String {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var _nfd: Unicode._NFD {
    Unicode._NFD(base: self[...].unicodeScalars)
  }
}

extension Substring {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var _nfd: Unicode._NFD {
    Unicode._NFD(base: unicodeScalars)
  }
}

//===----------------------------------------------------------------------===//
// Unicode.NFC
//===----------------------------------------------------------------------===//

extension Unicode {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public struct _NFC {
    let base: Substring.UnicodeScalarView
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFC {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public struct Iterator {
    var base: Unicode._InternalNFC<Substring>.Iterator
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFC.Iterator: IteratorProtocol {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public mutating func next() -> Unicode.Scalar? {
    base.next()
  }
}

@available(SwiftStdlib 5.7, *)
extension Unicode._NFC: Sequence {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public func makeIterator() -> Iterator {
    Iterator(
      base: Unicode._InternalNFC<Substring>.Iterator(
        iterator: Unicode._InternalNFD<Substring>(base: base).makeIterator()
      )
    )
  }
}

extension String {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var _nfc: Unicode._NFC {
    Unicode._NFC(base: self[...].unicodeScalars)
  }
}

extension Substring {
  @_spi(_Unicode)
  @available(SwiftStdlib 5.7, *)
  public var _nfc: Unicode._NFC {
    Unicode._NFC(base: unicodeScalars)
  }
}
