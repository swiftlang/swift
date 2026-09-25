// Each primary in a batch gets its own log, holding only its own diagnostics.

// RUN: %empty-directory(%t)

// One -serialize-diagnostics-path per primary, in primary order.
// RUN: %target-swift-frontend -typecheck -serialize-diagnostics=sarif \
// RUN:   -primary-file %s -serialize-diagnostics-path %t/main.sarif \
// RUN:   -primary-file %S/Inputs/sarif-diagnostics-batch-mode-helper.swift \
// RUN:     -serialize-diagnostics-path %t/helper.sarif \
// RUN:   %S/Inputs/sarif-diagnostics-batch-mode-other.swift

// RUN: %normalize_sarif %t/main.sarif | %diff -U1 -b %S/Inputs/expected-sarif/sarif-diagnostics-batch-mode.sarif -

// Same shape, so check only what distinguishes the helper's log.
// RUN: %FileCheck --input-file=%t/helper.sarif %s -check-prefix=HELPER
// HELPER: "uri" : "file:///{{.*}}/sarif-diagnostics-batch-mode-helper.swift"
// HELPER: "text" : "initialization of immutable value 'helperUnused' was never used
// HELPER-NOT: mainUnused
// HELPER-NOT: shouldNotShowUpInOutput

// A primary cut short by an error in another primary gets a zero-byte log, which
// is how the driver spots an incomplete compilation without parsing it.
// RUN: echo 'let bad: Int = "oops"' > %t/bad.swift
// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=sarif \
// RUN:   -primary-file %t/bad.swift -serialize-diagnostics-path %t/bad.sarif \
// RUN:   -primary-file %S/Inputs/sarif-diagnostics-batch-mode-helper.swift \
// RUN:     -serialize-diagnostics-path %t/cutshort.sarif
// RUN: test -e %t/cutshort.sarif -a ! -s %t/cutshort.sarif

// REQUIRES: swift_sarif

func mainFunction() {
  let mainUnused = 1
}
