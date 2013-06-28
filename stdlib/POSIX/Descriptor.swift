class Descriptor : Object {
  var fd : Int32

  destructor {
    var e = c_file_close(fd)
    alwaysTrap(e == 0)
  }
}
