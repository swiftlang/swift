//===----------------------------------------------------------------------===//
// Swift File Type
//===----------------------------------------------------------------------===//

class File {
  var fd : Int32
  var sz : Int

  func size() -> Int {return sz}

  func open(filename : String) {
    sz = c_file_size(filename.str_value.base.value)
    fd = c_file_open(filename.str_value.base.value)
    assert(fd != 0)
  }

  func close() {
    assert(fd != 0)
    var e = c_file_close(fd)
    assert(e == 0)
    fd = 0
    sz = 0
  }
}

extension String {
  constructor (f : File) {
    var buf : StringByte[] = new StringByte[f.size()]
    var sz = c_file_read(f.fd, buf.base.value, f.size())
    str_value = SliceStringByte.convertFromHeapArray(buf.base.value, buf.owner, f.size().value)
  }
}
