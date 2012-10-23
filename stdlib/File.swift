//===----------------------------------------------------------------------===//
// Swift File Type
//===----------------------------------------------------------------------===//

class File {
  var fd : Int32
  var body : String
  var size : Int

  constructor (filename : String) {
    open(filename)
  }
  destructor {
    if fd != 0 {
      close()
    }
  }

  func open(filename : String) {
    size = c_file_size(filename.str_value.base.value)
    fd = c_file_open(filename.str_value.base.value)
    assert(fd != 0)

    var buf : StringByteData = StringByteData.getNew(size)
    var sz = c_file_read(fd, buf.base.value, size)
    body.str_value = buf
  }

  func close() {
    assert(fd != 0)
    var e = c_file_close(fd)
    assert(e == 0)
    fd = 0
    size = 0
  }

  // FIXME: This is pretty terrible.
  func replPrint() {
    print("File{\n  fd=\(fd)\n  body=\"\(body)\"\n}")
  }
}

extension String {
  constructor (f : File) {
    this = f.body
  }
}

extension File {
  var lines : String[] {
    get {
      return body.splitIf({ $0 == '\n' || $0 == '\r' })
    }
  }
}

// New File design

// Open modes
// FIXME:  These should be nested enums of Ofile and IOfile
var error_if_doesnt_exist : Int32 = 0
var truncate : Int32              = 0x0200 | 0x0400
var append : Int32                = 0x0200 | 0x0008
var error_if_exists : Int32       = 0x0200 | 0x0400 | 0x0800

class Ifile {
  var _fd : Int32
  var _eof : Bool

  constructor () {
    _fd = 0
    _eof = true
  }

  constructor (filename : String) {
    _fd = 0
    _eof = true
    open(filename)
  }

  destructor {
    if _fd != 0 {
      close()
    }
  }

  func open(filename : String) {
    assert(_fd == 0)
    _fd = posix_open(filename.str_value.base.value, Int32(0x0000), 0o666)
    assert(_fd != 0)
    _eof = size == 0
  }

  func close() {
    assert(_fd != 0)
    posix_close(_fd)
    _fd = 0
    _eof = true
  }

  func getPosition() -> Int {
    assert(_fd != 0)
    return posix_seek(_fd, 0, 1)
  }

  func setPosition(p : Int) {
    assert(_fd != 0)
    var sz = posix_seek(_fd, 0, 2)
    assert(sz >= 0)
    if p >= sz {
      p = sz
      _eof = true
    }
    else {
      _eof = false
    }
    p = posix_seek(_fd, p, 0)
    assert(p >= 0)
  }

  func eof() -> Bool {
    return _eof
  }

  var size : Int {
    get {
      assert(_fd != 0)
      var cp = posix_seek(_fd, 0, 1)
      assert(cp >= 0)
      var sz = posix_seek(_fd, 0, 2)
      assert(sz >= 0)
      cp = posix_seek(_fd, cp, 0)
      assert(cp >= 0)
      return sz
    }
  }

  func read(buf : UInt8[]) -> Int {
    assert(_fd != 0)
    var r = posix_read(_fd, buf.base.value, buf.length)
    assert(r >= 0)
    _eof = r < buf.length
    return r
  }

  func read() -> Int {
    assert(_fd != 0)
    var c = new UInt8[1]
    if read(c) != 1 {
      _eof = true
      return -1
    }
    return Int(c[0])
  }
}

extension Ifile {
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

class Ofile {
  var _fd : Int32

  constructor () {
    _fd = 0
  }

  constructor (filename : String, mode : Int32) {
    _fd = 0
    open(filename, mode)
  }

  destructor {
    if _fd != 0 {
      close()
    }
  }

  func open(filename : String, mode : Int32) {
    assert(_fd == 0)
    _fd = posix_open(filename.str_value.base.value, Int32(0x0001) | mode, 0o666)
    assert(_fd != 0)
  }

  func close() {
    assert(_fd != 0)
    posix_close(_fd)
    _fd = 0
  }

  func getPosition() -> Int {
    assert(_fd != 0)
    return posix_seek(_fd, 0, 1)
  }

  func setPosition(p : Int) {
    assert(_fd != 0)
    var sz = posix_seek(_fd, 0, 2)
    assert(sz >= 0)
    if p >= sz {
      p = sz
    }
    p = posix_seek(_fd, p, 0)
    assert(p >= 0)
  }

  var size : Int {
    get {
      assert(_fd != 0)
      var cp = posix_seek(_fd, 0, 1)
      assert(cp >= 0)
      var sz = posix_seek(_fd, 0, 2)
      assert(sz >= 0)
      cp = posix_seek(_fd, cp, 0)
      assert(cp >= 0)
      return sz
    }
  }

  func write(buf : UInt8[]) -> Int {
    assert(_fd != 0)
    var r = posix_write(_fd, buf.base.value, buf.length)
    assert(r != -1)
    return r
  }

  func write(buf : String) -> Int {
    assert(_fd != 0)
    var r = posix_write(_fd, buf.str_value.base.value, buf.length)
    assert(r != -1)
    return r
  }

  func write(c : UInt8) {
    assert(_fd != 0)
    var buf = new UInt8[1]
    buf[0] = c
    var r = write(buf)
    assert(r == 1)
  }
}

class IOfile {
  var _fd : Int32
  var _eof : Bool

  constructor () {
    _fd = 0
    _eof = true
  }

  constructor (filename : String) {
    _fd = 0
    _eof = true
    open(filename)
  }

  constructor (filename : String, mode : Int32) {
    _fd = 0
    _eof = true
    open(filename, mode)
  }

  destructor {
    if _fd != 0 {
      close()
    }
  }

  func open(filename : String) {
    open(filename, error_if_doesnt_exist)
  }

  func open(filename : String, mode : Int32) {
    assert(_fd == 0)
    _fd = posix_open(filename.str_value.base.value, Int32(0x0002) | mode, 0o666)
    assert(_fd != 0)
    _eof = size == 0
  }

  func close() {
    assert(_fd != 0)
    posix_close(_fd)
    _fd = 0
    _eof = true
  }

  func getPosition() -> Int {
    assert(_fd != 0)
    return posix_seek(_fd, 0, 1)
  }

  func setPosition(p : Int) {
    assert(_fd != 0)
    var sz = posix_seek(_fd, 0, 2)
    assert(sz >= 0)
    if p >= sz {
      p = sz
      _eof = true
    }
    else {
      _eof = false
    }
    p = posix_seek(_fd, p, 0)
    assert(p >= 0)
  }

  func eof() -> Bool {
    return _eof
  }

  var size : Int {
    get {
      assert(_fd != 0)
      var cp = posix_seek(_fd, 0, 1)
      assert(cp >= 0)
      var sz = posix_seek(_fd, 0, 2)
      assert(sz >= 0)
      cp = posix_seek(_fd, cp, 0)
      assert(cp >= 0)
      return sz
    }
  }

  func read(buf : UInt8[]) -> Int {
    assert(_fd != 0)
    var r = posix_read(_fd, buf.base.value, buf.length)
    assert(r >= 0)
    _eof = r < buf.length
    return r
  }

  func read() -> Int {
    assert(_fd != 0)
    var c = new UInt8[1]
    if read(c) != 1 {
      _eof = true
      return -1
    }
    return Int(c[0])
  }

  func write(buf : UInt8[]) -> Int {
    assert(_fd != 0)
    var r = posix_write(_fd, buf.base.value, buf.length)
    assert(r != -1)
    if r > 0 {
      _eof = false
    }
    return r
  }

  func write(buf : String) -> Int {
    assert(_fd != 0)
    var r = posix_write(_fd, buf.str_value.base.value, buf.length)
    assert(r != -1)
    return r
  }

  func write(c : UInt8) {
    assert(_fd != 0)
    var buf = new UInt8[1]
    buf[0] = c
    var r = write(buf)
    assert(r == 1)
    _eof = false
  }
}

extension IOfile {
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
