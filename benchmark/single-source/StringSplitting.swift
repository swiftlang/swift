//===--- StringSplitting.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
	  name: "LineSink.bytes.alpha",
	  runFunction: run_LinkSink_bytes_alpha,
	  tags: [.validation, .String],
	  setUpFunction: setup),
  BenchmarkInfo(
	  name: "LineSink.bytes.complex",
	  runFunction: run_LinkSink_bytes_complex,
	  tags: [.validation, .String],
	  setUpFunction: setup),
  BenchmarkInfo(
	  name: "LineSink.scalars.alpha",
	  runFunction: run_LinkSink_scalars_alpha,
	  tags: [.validation, .String],
	  setUpFunction: setup),
  BenchmarkInfo(
	  name: "LineSink.scalars.complex",
	  runFunction: run_LinkSink_scalars_complex,
	  tags: [.validation, .String],
	  setUpFunction: setup),
	BenchmarkInfo(
	  name: "LineSink.characters.alpha",
	  runFunction: run_LinkSink_characters_alpha,
	  tags: [.validation, .String],
	  setUpFunction: setup),
  BenchmarkInfo(
	  name: "LineSink.characters.complex",
	  runFunction: run_LinkSink_characters_complex,
	  tags: [.validation, .String],
	  setUpFunction: setup),
]


// Line-sink benchmarks: Implement `lines`-like functionality
enum View {
  case character
  case scalar
  case utf8
}

@inline(__always) // Constant fold the switch away, inline closures
fileprivate func lineSink(_ workload: String, view: View, sink: (String) -> ()) {
  switch view {
    case .character:
      var iter = workload.makeIterator()
      _linesByCharacters(source: { iter.next() }, sink: sink)
    case .scalar:
      var iter = workload.unicodeScalars.makeIterator()
      _linesByScalars(source: { iter.next() }, sink: sink)
    case .utf8:
      var iter = workload.utf8.makeIterator()
      _linesByBytes(source: { iter.next() }, sink: sink)
  }
}


// Inline always to try to ignore any closure stuff
@inline(__always)
fileprivate func _linesByCharacters(
  source characterSource: () throws -> Character?,
  sink lineSink: (String) -> ()
) rethrows {
  var buffer = ""
  func yield() {
    lineSink(buffer)
    buffer.removeAll(keepingCapacity: true)
  }
  while let c = try characterSource() {
    if c.isNewline {
      yield()
    } else {
      buffer.append(c)
    }
  }

  // Don't emit an empty newline when there is no more content (e.g. end of file)
  if !buffer.isEmpty {
    yield()
  }
}

// Inline always to try to ignore any closure stuff
@inline(__always)
fileprivate func _linesByScalars(
  source scalarSource: () throws -> Unicode.Scalar?,
  sink lineSink: (String) -> ()
) rethrows {
  guard #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) else {
    fatalError("unavailable")
  }

  var buffer: Array<UInt8> = []
  func yield() {
    lineSink(String(decoding: buffer, as: UTF8.self))
    buffer.removeAll(keepingCapacity: true)
  }

  let _CR = Unicode.Scalar(0x0D)!
  let _LF = Unicode.Scalar(0x0A)!

  while let first = try scalarSource() {
    switch first.value {
    case _CR.value:
      yield()
      guard let second = try scalarSource() else { return }
      if second != _LF { buffer.append(contentsOf: second.utf8) }
    case 0x000A..<0x000D /* LF ..< CR */: yield()
    case 0x0085 /* NEXT LINE (NEL) */: yield()
    case 0x2028 /* LINE SEPARATOR */: yield()
    case 0x2029 /* PARAGRAPH SEPARATOR */: yield()
    default: buffer.append(contentsOf: first.utf8)
    }
  }

  // Don't emit an empty newline when there is no more content (e.g. end of file)
  if !buffer.isEmpty {
    yield()
  }
}


@inline(__always)
fileprivate func _linesByBytes(
  source byteSource: () throws -> UInt8?,
  sink lineSink: (String) -> ()
) rethrows {

  let _CR: UInt8 = 0x0D
  let _LF: UInt8 = 0x0A

  var buffer: Array<UInt8> = []
  func yield() {
    lineSink(String(decoding: buffer, as: UTF8.self))
    buffer.removeAll(keepingCapacity: true)
  }

  /*
    0D 0A: CR-LF
    0A | 0B | 0C | 0D: LF, VT, FF, CR
    E2 80 A8:  U+2028 (LINE SEPARATOR)
    E2 80 A9:  U+2029 (PARAGRAPH SEPARATOR)
  */
  while let first = try byteSource() {
    switch first {
    case _CR:
      yield()
      // Swallow up any subsequent LF
      guard let next = try byteSource() else { return }
      if next != _LF { buffer.append(next) }
    case 0x0A..<0x0D: yield()

    case 0xE2:
      // Try to read: 80 [A8 | A9].
      // If we can't, then we put the byte in the buffer for error correction
      guard let next = try byteSource() else {
        buffer.append(first)
        yield()
        return
      }
      guard next == 0x80 else {
        buffer.append(first)
        buffer.append(next)
        continue
      }
      guard let fin = try byteSource() else {
        buffer.append(first)
        buffer.append(next)
        yield()
        return
      }
      guard fin == 0xA8 || fin == 0xA9 else {
        buffer.append(first)
        buffer.append(next)
        buffer.append(fin)
        continue
      }
      yield()
    default:
      buffer.append(first)
    }
  }
  // Don't emit an empty newline when there is no more content (e.g. end of file)
  if !buffer.isEmpty {
    yield()
  }
}

// 1-byte sequences
private let ascii = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."

// 2-byte sequences
private let russian = "Ру́сский язы́к один из восточнославянских языков, национальный язык русского народа."
// 3-byte sequences
private let japanese = "日本語（にほんご、にっぽんご）は、主に日本国内や日本人同士の間で使われている言語である。"
// 4-byte sequences
// Most commonly emoji, which are usually mixed with other text.
private let emoji = "Panda 🐼, Dog 🐶, Cat 🐱, Mouse 🐭."

private let longComplexNewlines: String = {
  var longStr = ascii + russian + japanese + emoji + "\n"
  var str = longStr
  str += longStr.split(separator: " ").joined(separator: "\n")
  str += longStr.split(separator: " ").joined(separator: "\r\n")
  str += longStr.split(separator: " ").joined(separator: "\r")
  str += longStr.split(separator: " ").joined(separator: "\u{0B}")
  str += longStr.split(separator: " ").joined(separator: "\u{0C}")
  str += longStr.split(separator: " ").joined(separator: "\u{2028}")
  str += longStr.split(separator: " ").joined(separator: "\u{2029}")
  str += "\n\n"
  return str
}()

public func run_LinkSink_bytes_alpha(_ n: Int) {
  let str = alphaInteriorNewlines
  for _ in 0..<(n*50) {
    lineSink(str, view: .utf8, sink: blackHole)
  }
}
public func run_LinkSink_bytes_complex(_ n: Int) {
  let str = longComplexNewlines
  for _ in 0..<n {
    lineSink(str, view: .utf8, sink: blackHole)
  }
}
public func run_LinkSink_scalars_alpha(_ n: Int) {
  let str = alphaInteriorNewlines
  for _ in 0..<(n*50) {
    lineSink(str, view: .scalar, sink: blackHole)
  }
}
public func run_LinkSink_scalars_complex(_ n: Int) {
  let str = longComplexNewlines
  for _ in 0..<n {
    lineSink(str, view: .utf8, sink: blackHole)
  }
}
public func run_LinkSink_characters_alpha(_ n: Int) {
  let str = alphaInteriorNewlines
  for _ in 0..<(n*50) {
    lineSink(str, view: .character, sink: blackHole)
  }
}
public func run_LinkSink_characters_complex(_ n: Int) {
  let str = longComplexNewlines
  for _ in 0..<n {
    lineSink(str, view: .character, sink: blackHole)
  }
}

fileprivate func setup() {
  var utf8Alpha: Array<String> = []
  lineSink(alphaInteriorNewlines, view: .utf8) { utf8Alpha.append($0) }

  var scalarAlpha: Array<String> = []
  lineSink(alphaInteriorNewlines, view: .scalar) { scalarAlpha.append($0) }

  var characterAlpha: Array<String> = []
  lineSink(alphaInteriorNewlines, view: .character) { characterAlpha.append($0) }

  check(utf8Alpha == scalarAlpha)
  check(utf8Alpha == characterAlpha)

  var utf8Complex: Array<String> = []
  lineSink(longComplexNewlines, view: .utf8) { utf8Complex.append($0) }

  var scalarComplex: Array<String> = []
  lineSink(longComplexNewlines, view: .scalar) { scalarComplex.append($0) }

  var characterComplex: Array<String> = []
  lineSink(longComplexNewlines, view: .character) { characterComplex.append($0) }

  check(utf8Complex == scalarComplex)
  check(utf8Complex == characterComplex)

  print("preconditions checked")
}


private let alphaInteriorNewlines: String =
  """
  abc\(Unicode.Scalar(0x0A)!    // LF
  )def\(Unicode.Scalar(0x0B)!   // VT
  )ghi\(Unicode.Scalar(0x0C)!   // FF
  )jkl\(Unicode.Scalar(0x0D)!   // CR
  )mno\(Unicode.Scalar(0x0D)!)\(Unicode.Scalar(0x0A)!   // CR-LF
  )pqr\(Unicode.Scalar(0x2028)! // LS
  )stu\(Unicode.Scalar(0x2029)! // PS
  )vwx
  yz
  """


