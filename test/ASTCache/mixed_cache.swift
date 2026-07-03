// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: cp %s %t/a.swift
// RUN: echo 'struct B { var y: Int }' > %t/b.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %t/a.swift %t/b.swift 2>&1 | FileCheck %s --check-prefix=COLD
// RUN: rm %t.cache/testmod/*.swiftast
// RUN: rm %t/b.swift
// RUN: echo 'struct B { var z: Int }' > %t/b.swift
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %t/a.swift %t/b.swift 2>&1 | FileCheck %s --check-prefix=PARTIAL

// COLD-DAG: AST cache: MISS (no cache file) for {{.*}}a.swift
// COLD-DAG: AST cache: MISS (no cache file) for {{.*}}b.swift
// COLD-DAG: AST cache: SAVED for {{.*}}a.swift
// COLD-DAG: AST cache: SAVED for {{.*}}b.swift

// PARTIAL: AST cache: MISS (no cache file) for {{.*}}b.swift
// PARTIAL-NOT: AST cache: HIT
// PARTIAL-NOT: AST cache: SAVED

// Test C2: If any file in a module is missing a cache entry, all files
// skip the cache (no mixed cached/uncached state).
struct A {
  var x: Int
}
