// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: cp %s %t.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -typecheck %t.swift 2>&1 | FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -typecheck %t.swift 2>&1 | FileCheck %s --check-prefix=SECOND
// Modify the file and verify cache miss.
// RUN: sed -i.bak 's/42/99/' %t.swift && rm %t.swift.bak
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -typecheck %t.swift 2>&1 | FileCheck %s --check-prefix=THIRD

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// THIRD: AST cache: MISS (invalid)
// THIRD: AST cache: SAVED

// Test that modifying a source file invalidates the cache.
let x = 42
