// RUN: %target-typecheck-verify-swift

// SR-4205: Don't warn about non-trailing closures followed by parameters with
// default arguments.
func f1(_ f: () -> (), bar: Int = 10) { } // no-warning
