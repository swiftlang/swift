// Ensure that an error in a primary is reflected in the right .dia file and
// that it causes other .dia files to be truncated, signalling incomplete compilation.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia  2> %t.stderr.txt
// RUN: c-index-test -read-diagnostics %t.main.dia 2> %t.main.txt

// RUN: %FileCheck -check-prefix=ERROR %s <%t.main.txt
// RUN: test -e %t.empty.dia -a ! -s %t.empty.dia

// ERROR: error:

func test(x: SomeType) {
  nonexistent()
}
