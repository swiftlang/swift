//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Returns `Characters` read from standard input through the end of the
/// current line or until EOF is reached, or `nil` if EOF has already been
/// reached.
///
/// If `stripNewline` is `true`, newline characters and character
/// combinations will be stripped from the result.  This is the default.
///
/// Standard input is interpreted as `UTF-8`.  Invalid bytes
/// will be replaced by Unicode `replacement characters
/// <http://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character>`_
public func readLine(stripNewline: Bool = true) -> String? {
  var linePtr: UnsafeMutablePointer<CChar> = nil
  let readBytes = swift_readLine_stdin(&linePtr)
  if readBytes == Int(bitPattern: UInt.max) {
    return nil
  }
  if stripNewline {
    // FIXME: implement
  }
  let result = String._fromCodeUnitSequenceWithRepair(UTF8.self,
    input: UnsafeMutableBufferPointer(
      start: UnsafeMutablePointer<UTF8.CodeUnit>(linePtr),
      count: Int(readBytes))).0
  free(linePtr)
  return result
}

