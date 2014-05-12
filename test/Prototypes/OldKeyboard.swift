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
// InputStream
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

// XXX FIXME -- replace and flush this out with parsing logic

@final class Keyboard {
  init() {  }

  func read(inout buf: UInt8[]) -> Int {
    buf.reserve(0) // ensure uniqueness before we allow buf to be modified
    var r = posix_read(0, buf.elementStorage.value, buf.count)
    if r < 0 {
      fatal("read failed")
    }
    return r
  }

  func read() -> Int {
    var c = new UInt8[1]
    if read(&c) != 1 {
      return -1
    }
    return Int(c[0])
  }
}

extension Keyboard {
  func getline() -> String {
    return getline("\n")
  }

  func getline(delim: Character) -> String {
    var r = String()
    var i = read()
    while i != -1 {
      var c = Character(UnicodeScalar(i))
      if c == delim {
        break
      }
      r = r + c
      i = read()
    }
    return r
  }
}

var kbd : Keyboard = Keyboard()
