// Classic C assert(), plus a message.
func assert(fn : [auto_closure] () -> Bool, message : String = "") {
}

// These trap functions promise to emit the debugger trap instruction and not
// print anything; therefore these functions clobber far fewer registers and
// are more useful for debugging and automated crash reporting infrastructure.

// "Expensive" asserts that are debug only
func debugTrap(fn : [auto_closure] () -> Bool = false, message : String = "") {
}

// "Important" and/or cheap asserts that are always enabled
func alwaysTrap(cond : Bool = false, message : String = "") {
  if !cond {
    Builtin.int_trap()
  }
}
