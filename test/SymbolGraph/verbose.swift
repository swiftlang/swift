// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name verbose -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name verbose -I %t -pretty-print -output-dir %t | %FileCheck %s -check-prefix=QUIET --allow-empty
// RUN: %target-swift-symbolgraph-extract -module-name verbose -I %t -pretty-print -output-dir %t -v 2>&1 | %FileCheck %s -check-prefix=VERBOSE

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name verbose -emit-module -emit-module-path %t/verbose.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t | %FileCheck %s -check-prefix=DRIVER --allow-empty

// rdar://76461340
// REQUIRES: rdar76461340

// QUIET-NOT: Emitting symbol graph for module file
// QUIET-NOT: 2 top-level declarations in this module.
// QUIET-NOT: Found 1 symbols and 0 relationships.

// VERBOSE: Emitting symbol graph for module file
// VERBOSE: 2 top-level declarations in this module.
// VERBOSE: Found 1 symbols and 0 relationships.

// DRIVER-NOT: Emitting symbol graph for module file
// DRIVER-NOT: 2 top-level declarations in this module.
// DRIVER-NOT: Found 1 symbols and 0 relationships.

public func someFunc() {}
