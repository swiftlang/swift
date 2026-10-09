// The dependency scanner serializes its diagnostics in the requested format.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// The dependency scanner sets up its own diagnostic consumer.
// RUN: %swift-scan-test -action scan_dependency -- %target-swift-frontend -parse-stdlib %t/test.swift \
// RUN:   -serialize-diagnostics=sarif -serialize-diagnostics-path %t/scan.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-dependency-scan.sarif < %t/scan.sarif

// A scan run directly by the frontend writes the same log.
// RUN: not %target-swift-frontend -scan-dependencies -parse-stdlib %t/test.swift \
// RUN:   -serialize-diagnostics=sarif -serialize-diagnostics-path %t/standalone.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-dependency-scan.sarif < %t/standalone.sarif

// Without a format, the scan's log stays in the binary format.
// RUN: %swift-scan-test -action scan_dependency -- %target-swift-frontend -parse-stdlib %t/test.swift \
// RUN:   -serialize-diagnostics-path %t/scan.dia
// RUN: c-index-test -read-diagnostics %t/scan.dia 2>&1 | %FileCheck %s -check-prefix=BITCODE

// BITCODE: test.swift:1:8: error: unable to resolve module dependency: 'DoesNotExist'
// BITCODE: test.swift:1:8: note: a dependency of main module 'test'

// REQUIRES: swift_sarif

//--- test.swift
import DoesNotExist
