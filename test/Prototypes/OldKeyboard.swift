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

@final class Console {
  init() { }

  func write(inout buf: UInt8[]) -> Int {
    let count = buf.count
    var r = 0
    for var start = 0; start < count; start += 1024 {
      let slice = buf[start...min(start + 1024, count)]
      r = slice.withUnsafePointerToElements {
        posix_write(1, $0.value, slice.count)
      }
      if r == -1 {
        break
      }
    }
    return r
  }

  func write(buf: String) -> Int {
    var r = 0
    let p = buf.contiguousUTF8

    if p != nil {
      let r = posix_write(1, p.value, buf.core.count)
    }
    else {
      var a = NativeArray<UInt8>(count: 1024, value: 0)
      var count = 0

      for u in buf.utf8 {
        a[count++] = u
        if count == a.count {
          r = a.withUnsafePointerToElements {
            posix_write(1, $0.value, count)
          }
          if r == -1 {
            break
          }
        }
      }

      if count != 0 {
        r = a.withUnsafePointerToElements {
          posix_write(1, $0.value, count)
        }
      }
    }

    securityCheck(r != -1)
    return r
  }

  func write(c: UInt8) {
    var buf = new UInt8[1]
    buf[0] = c
    var r = write(&buf)
    securityCheck(r == 1)
  }
}

var con: Console = Console()


