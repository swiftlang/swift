func assert(fn : [auto_closure] () -> Bool) {
  if !fn() {
    Builtin.trap()
  }
}
