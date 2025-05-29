//===--- OSReleaseScanner.swift --------------------------------*- swift -*-===//
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
//
//  Defines OSReleaseScanner, which is for scanning the /etc/os-release
//  file on Linux.
//
//===----------------------------------------------------------------------===//

#if os(Linux)

import Swift

// Lines in /etc/os-release consist of KEY=VALUE pairs.
//
// The VALUE may be quoted with single quotes, in which case its contents
// are left alone.
//
// It may also be quoted with double quotes, in which case slash escapes
// are processed.
//
// If it is unquoted, whitespace will be stripped.

struct OSReleaseScanner<S: StringProtocol>: Sequence, IteratorProtocol {
  typealias SS = S.SubSequence

  private enum State {
    case normal
    case badLine
    case comment
    case key
    case beforeEquals
    case beforeValue
    case value
    case valueWhitespace
    case singleQuote
    case doubleQuote
    case escape
    case awaitingNewline
  }

  private var asString: S
  private var asUTF8: S.UTF8View
  private var pos: S.UTF8View.Index
  private var state: State

  init(_ string: S) {
    asString = string
    asUTF8 = string.utf8
    pos = asUTF8.startIndex
    state = .normal
  }

  mutating func next() -> (String, String)? {
    var chunkStart = pos
    var whitespaceStart = pos
    var key: String = ""
    var quotedValue: String = ""

    while pos < asUTF8.endIndex {
      let ch = asUTF8[pos]
      switch state {
        case .normal:
          if ch == 32 || ch == 9 || ch == 13 || ch == 10 {
            break
          }
          if ch == UInt8(ascii: "#") {
            state = .comment
            break
          }
          chunkStart = pos
          state = .key
        case .badLine, .comment, .awaitingNewline:
          if ch == 13 || ch == 10 {
            state = .normal
          }
        case .key:
          if ch == 32 || ch == 9 {
            key = String(asString[chunkStart..<pos])
            state = .beforeEquals
            break
          }
          if ch == 13 || ch == 10 {
            state = .normal
            break
          }
          if ch == UInt8(ascii: "=") {
            key = String(asString[chunkStart..<pos])
            state = .beforeValue
            break
          }
        case .beforeEquals:
          if ch == UInt8(ascii: "=") {
            state = .beforeValue
            break
          }
          if ch == 32 || ch == 9 {
            break
          }
          state = .badLine
        case .beforeValue:
          if ch == 32 || ch == 9 {
            break
          }
          if ch == UInt8(ascii: "\"") {
            state = .doubleQuote
            chunkStart = asUTF8.index(after: pos)
            quotedValue = ""
            break
          }
          if ch == UInt8(ascii: "'") {
            state = .singleQuote
            chunkStart = asUTF8.index(after: pos)
            break
          }
          chunkStart = pos
          state = .value
        case .value:
          if ch == 13 || ch == 10 {
            let value = String(asString[chunkStart..<pos])
            state = .normal
            return (key, value)
          }
          if ch == 32 || ch == 9 {
            state = .valueWhitespace
            whitespaceStart = pos
          }
        case .valueWhitespace:
          if ch == 13 || ch == 10 {
            let value = String(asString[chunkStart..<whitespaceStart])
            state = .normal
            return (key, value)
          }
          if ch != 32 && ch != 9 {
            state = .value
          }
        case .singleQuote:
          if ch == UInt8(ascii: "'") {
            let value = String(asString[chunkStart..<pos])
            state = .awaitingNewline
            return (key, value)
          }
        case .doubleQuote:
          if ch == UInt8(ascii: "\\") {
            let chunk = String(asString[chunkStart..<pos])
            quotedValue += chunk
            chunkStart = asUTF8.index(after: pos)
            state = .escape
            break
          }
          if ch == UInt8(ascii: "\"") {
            let chunk = String(asString[chunkStart..<pos])
            quotedValue += chunk
            state = .awaitingNewline
            return (key, quotedValue)
          }
        case .escape:
          let toEscape = asString[chunkStart...pos]
          switch toEscape {
            case "n":
              quotedValue += "\n"
            case "t":
              quotedValue += "\t"
            default:
              quotedValue += toEscape
          }
          chunkStart = asUTF8.index(after: pos)
          state = .doubleQuote
      }

      pos = asUTF8.index(after: pos)
    }

    return nil
  }
}

#endif // os(Linux)
