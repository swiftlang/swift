// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift 2> %t/b.hash
// RUN: not cmp %t/a.hash %t/b.hash

/// We should generate an interface hash for emit-module-separately jobs even
/// with no primaries.
// RUN: %target-swift-frontend -dump-interface-hash %t/b.swift -experimental-skip-non-inlinable-function-bodies-without-types 2> %t/b-emit-module.hash
// RUN: cmp %t/b.hash %t/b-emit-module.hash

// BEGIN a.swift
func f() {}

// BEGIN b.swift
func f() {}
func g() {}
