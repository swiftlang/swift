class Descriptor {
  var fd : Int32

  destructor {
    debugTrap(fd != 0)
    var e = c_file_close(fd)
    alwaysTrap(e == 0)
  }
}
