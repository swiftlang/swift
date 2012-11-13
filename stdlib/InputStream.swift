//===----------------------------------------------------------------------===//
// InputStream
//===----------------------------------------------------------------------===//

// XXX FIXME -- replace and flush this out with parsing logic

class Keyboard {
  func read(buf : UInt8[]) -> Int {
    var r = posix_read(0, buf.base.value, buf.length)
    assert(r >= 0)
    return r
  }

  func read() -> Int {
    var c = new UInt8[1]
    if read(c) != 1 {
      return -1
    }
    return Int(c[0])
  }
}

extension Keyboard {
  func getline() -> String {
    return getline('\n')
  }

  func getline(delim : Char) -> String {
    var r : String
    var i = read()
    while i != -1 {
      var c = Char(i)
      if c == delim {
        break
      }
      r = r + c
      i = read()
    }
    return r
  }
}

var kbd : Keyboard = new Keyboard
