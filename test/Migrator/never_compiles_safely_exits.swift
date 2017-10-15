// For files that can never compile, make sure we don't crash when
// attempting to do some pre-fixit runs early in the Migrator pipeline.
// Since this code isn't fixable, the invocation should exit with non-zero status.

// RUN: not %target-swift-frontend -typecheck -update-code -primary-file %s %S/Inputs/never_compiles_safely_exits_breaking_code.swift -emit-remap-file-path %t/never_compiles_safely_exits.remap -emit-migrated-file-path %t/never_compiles_safely_exits.result -swift-version 4
// RUN: not ls %t/never_compiles_safely_exits.swift.result
// RUN: not ls %t/never_compiles_safely_exits.swift.remap

func bar() {
  foo(bar)
}
