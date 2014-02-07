// RUN: not %swift -repl %s 2>&1 | FileCheck -check-prefix=REPL_NO_FILES %s
// REPL_NO_FILES: REPL mode requires no input files
// REQUIRES: swift_repl
