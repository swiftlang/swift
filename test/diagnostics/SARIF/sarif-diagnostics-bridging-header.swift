// Diagnostics raised inside a bridging header are recorded against the header,
// as they are in the binary format. See ClangImporter/diags_from_header.swift.

// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -enable-objc-interop \
// RUN:   -import-objc-header %S/Inputs/sarif-diagnostics-bridging-header.h \
// RUN:   -serialize-diagnostics=sarif -serialize-diagnostics-path %t/diags.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-bridging-header.sarif < %t/diags.sarif

// In batch mode the header's diagnostics belong to no primary, so each primary's
// log gets all of them, and neither primary is treated as cut short.
// RUN: not %target-swift-frontend -typecheck -enable-objc-interop \
// RUN:   -import-objc-header %S/Inputs/sarif-diagnostics-bridging-header.h \
// RUN:   -serialize-diagnostics=sarif \
// RUN:   -primary-file %s -serialize-diagnostics-path %t/main.sarif \
// RUN:   -primary-file %S/../../Inputs/empty.swift \
// RUN:     -serialize-diagnostics-path %t/empty.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-bridging-header.sarif < %t/main.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-bridging-header.sarif < %t/empty.sarif

// REQUIRES: swift_sarif
