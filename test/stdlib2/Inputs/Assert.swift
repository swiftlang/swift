// Classic C assert() in every way
func assert(fn : [auto_closure] () -> Bool) {
}

// These trap functions promise to emit the debugger trap instruction and not
// print anything; therefore these functions clobber far fewer registers and
// are more useful for debugging and automated crash reporting infrastructure.

// "Expensive" asserts that are debug only
func debugTrap(fn : [auto_closure] () -> Bool = false) {
}

// "Important" and/or cheap asserts that are always enabled
func alwaysTrap(cond : Bool = false) {
  if !cond {
    Builtin.trap()
  }
}
