// Note: test of the scope map. All of these tests are line- and
// column-sensitive, so any additions should go at the end.

struct X {}
var y: () -> X

struct S {
  let x: () = y = {() -> X in
    return .init()
  }
}

// RUN: %target-swift-frontend -dump-scope-maps expanded %s 2>&1 | %FileCheck %s

// rdar://147751795 - Make sure we don't end up with a duplicate closure scope.
// CHECK:      PatternEntryInitializerScope {{.*}}, [8:15 - 10:3] entry 0 'x'
// CHECK-NEXT:   `-ClosureParametersScope {{.*}}, [8:28 - 10:3]
// CHECK-NEXT:     `-BraceStmtScope {{.*}}, [8:28 - 10:3]
// CHECK-EMPTY:
