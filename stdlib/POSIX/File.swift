class VFSObject : Descriptor {
}

class File : VFSObject {
  var body : String
  var size : Int

  constructor (filename : String) {
    size = c_file_size(filename.str_value.base.value)
    fd = c_file_open(filename.str_value.base.value)
    assert(fd != 0)

    var buf : StringByteData = StringByteData.getNew(size)
    var sz = c_file_read(fd, buf.base.value, size)
    body.str_value = buf
  }

  var lines : String[] {
    get {
      return body.splitIf({ $0 == '\n' || $0 == '\r' })
    }
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

class Directory : VFSObject {
}
