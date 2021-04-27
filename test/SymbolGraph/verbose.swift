// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name verbose -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name verbose -I %t -pretty-print -output-dir %t | %FileCheck %s -check-prefix=QUIET --allow-empty
// RUN: %target-swift-symbolgraph-extract -module-name verbose -I %t -pretty-print -output-dir %t -v 2>&1 | %FileCheck %s -check-prefix=VERBOSE

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name verbose -emit-module -emit-module-path %t/verbose.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t | %FileCheck %s -check-prefix=DRIVER --allow-empty

// QUIET-NOT: Emitting symbol graph for module file
// QUIET-NOT: {{[[:digit:]]}} top-level declarations in this module.
// QUIET-NOT: Found {{[[:digit:]]}} symbols and {{[[:digit:]]}} relationships.

// VERBOSE: Emitting symbol graph for module file
// VERBOSE: {{[[:digit:]]}} top-level declarations in this module.
// VERBOSE: Found {{[[:digit:]]}} symbols and {{[[:digit:]]}} relationships.

// DRIVER-NOT: Emitting symbol graph for module file
// DRIVER-NOT: {{[[:digit:]]}} top-level declarations in this module.
// DRIVER-NOT: Found {{[[:digit:]]}} symbols and {{[[:digit:]]}} relationships.

public func someFunc() {}
