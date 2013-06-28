class VFSObject : Descriptor {
  constructor (path : String) {
    var LM = LifetimeManager()
    fd = c_file_open(LM.getCString(path))
    LM.release()
    alwaysTrap(fd != -1)
  }
}

class File : VFSObject {
  var body : String
  var size : Int

  constructor (obj : VFSObject) {
    fd = posix_dup(obj.fd)
    size = c_fd_size(fd)

    var buf = StringByteData.getNew(size)
    var sz = c_file_read(fd, buf.base.value, size)
    body.str_value = buf
  }

  var lines : String[] {
  get:
    return body.splitIf({ $0 == '\n' || $0 == '\r' })
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
  var nodes : Vector<String>
  constructor (path : String) {
    nodes = Vector<String>()
    var handle = posix_opendir_hack(path.str_value.base.value)
    while true {
      var (cstr, clen) = posix_readdir_hack(handle)
      if clen == 0 {
        break
      }
      var noOwner : Builtin.ObjectPointer
      nodes.append(String(StringByteData.convertFromHeapArray(
        cstr, noOwner, clen.value)))
    }
    posix_closedir_hack(handle)
  }
}

func isDirectory(path : String) -> Bool {
  var rval = posix_isDirectory_hack(path.str_value.base.value)
  return rval == 1
}
