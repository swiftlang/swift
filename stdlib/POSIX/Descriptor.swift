class Descriptor {
  var fd : Int32

  destructor {
    assert(fd != 0)
    var e = c_file_close(fd)
    assert(e == 0)
  }
}
