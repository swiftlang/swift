// Classic C assert() in every way
func assert(fn : [auto_closure] () -> Bool) {
  if !fn() {
    // FIXME -- Should work like so:
    // fprintf(stderr, "assertion \"%s\" failed: file \"%s\", line %d\n",
    //                 "expression", __FILE__, __LINE__);
    print("assertion failed")
    abort()
  }
}

// These trap functions promise to emit the debugger trap instruction and not
// print anything; therefore these functions clobber far fewer registers and
// are more useful for debugging and automated crash reporting infrastructure.

// "Expensive" asserts that are debug only
func debugTrap(fn : [auto_closure] () -> Bool = false) {
  if !fn() {
    Builtin.trap()
  }
}

// "Important" and/or cheap asserts that are always enabled
func alwaysTrap(cond : Bool = false) {
  if !cond {
    Builtin.trap()
  }
}
