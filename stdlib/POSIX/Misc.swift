func [asmname="posix_get_errno"] posix_get_errno() -> Int32
func [asmname="posix_set_errno"] posix_set_errno(value : Int32)

var errno : Int32 {
  get {
    return posix_get_errno()
  }
  set(value) {
    posix_set_errno(value)
  }
}
