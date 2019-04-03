// Ensure that an error in a non-primary causes an error in the errorless primary.
//
// RUN: rm -f %t.*

// First, put the causal primary first:

// RUN: not %target-swift-frontend -typecheck  -primary-file %s  -serialize-diagnostics-path %t.main.dia -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt

// RUN: test -e %t.main.dia -a -s %t.main.dia
// RUN: test -e %t.empty.dia -a ! -s %t.empty.dia

// Now, put the causal primary second:
// (No matter which order, the error message goes with the causal primary.)

// RUN: not %target-swift-frontend -typecheck  -primary-file %S/../Inputs/empty.swift  -serialize-diagnostics-path %t.empty.dia -primary-file %s  -serialize-diagnostics-path %t.main.dia  %S/Inputs/serialized-diagnostics-batch-mode-suppression-helper.swift  2> %t.stderr.txt

// RUN: test -e %t.main.dia -a -s %t.main.dia
// RUN: test -e %t.empty.dia -a ! -s %t.empty.dia

func test(x: SomeType) {
}
