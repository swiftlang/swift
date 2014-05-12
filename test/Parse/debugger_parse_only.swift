// RUN: %swift %s -verify -debugger-support -parse
// This test is to make sure the parser allows imports in other than
// top-level code when debugger support is on.  The test only runs
// the parse phase because the debugger will remove the import decl's
// after parse, and the rest of the toolchain is not required to handle
// them without asserting (and in fact doesn't.)

// XFAIL: *

func imports_allowed () {
    import Swift
}
