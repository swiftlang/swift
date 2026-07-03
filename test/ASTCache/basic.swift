// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -typecheck %s 2>&1 | FileCheck %s --check-prefix=FIRST
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -typecheck %s 2>&1 | FileCheck %s --check-prefix=SECOND

// FIRST: AST cache: MISS (no cache file)
// FIRST: AST cache: SAVED

// SECOND: AST cache: HIT

// Test that per-file AST caching works for type-checking.
// Note: SIL lowering from cached AST is not yet supported (DeclContext
// ownership needs to be fixed for deserialized Decls).
struct Point {
  var x: Double
  var y: Double

  func distance(to other: Point) -> Double {
    let dx = x - other.x
    let dy = y - other.y
    return (dx * dx + dy * dy).squareRoot()
  }
}

let origin = Point(x: 0, y: 0)
let p = Point(x: 3, y: 4)
_ = origin.distance(to: p)
