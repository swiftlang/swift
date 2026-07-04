// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -typecheck %s 2>&1 | FileCheck %s --check-prefix=SECOND

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// Test that per-file AST caching works for type-checking.
// This file uses -parse-as-library to avoid script-mode (C4: TopLevelCodeDecl
// is not serialized, so script-mode files are excluded from caching).
struct Point {
  var x: Double
  var y: Double

  func distance(to other: Point) -> Double {
    let dx = x - other.x
    let dy = y - other.y
    return (dx * dx + dy * dy).squareRoot()
  }
}
