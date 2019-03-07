// Ensure that an error in a non-primary causes an error in the errorless primary.
//
// RUN: rm -f %t.*

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt

// RUN: test -e %t.main.dia -a ! -s %t.empty.dia
// RUN: test -e %t.empty.dia -a ! -s %t.empty.dia

func test(x: SomeType) {
}
