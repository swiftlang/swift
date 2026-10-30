// RUN: %target-swift-frontend -g -emit-sil %s -parse-as-library -module-name a | %FileCheck %s

// Instructions without a usable source location (cleanups, the epilogue) must
// not inherit the scope of a nested block that has already ended.
// rdar://191202669

@inline(never) func use<T>(_ t: T) {}
final class C {}

// The epilogue is outside of the nested block.
// CHECK-LABEL: sil @$s1a18epilogueAfterBlockyySbF
// CHECK: debug_value {{.*}}, let, name "flag", argno 1, {{.*}}, scope [[EAB_FN:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "inner", {{.*}}, scope [[EAB_INNER:[0-9]+]]
// CHECK: return {{.*}}, scope [[EAB_FN]]
public func epilogueAfterBlock(_ flag: Bool) {
  if flag {
    let inner = 42
    use(inner)
  }
}

// The epilogue is still inside the scope of a top-level binding.
// CHECK-LABEL: sil @$s1a11topLevelLetyySbF
// CHECK: debug_value {{.*}}, let, name "x", {{.*}}, scope [[TLL_X:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "inner", {{.*}}, scope [[TLL_INNER:[0-9]+]]
// CHECK: return {{.*}}, scope [[TLL_X]]
public func topLevelLet(_ flag: Bool) {
  let x = 1
  if flag {
    let inner = 42
    use(inner)
  }
}

// A guard binding outlives the guard statement.
// FIXME: The return is shared with the else branch, where v is not bound.
// Ideally in this case the return should use the function's scope to avoid
// showing potentially uninitialized variables.

// CHECK-LABEL: sil @$s1a8guardLetyySiSg_SbtF
// CHECK: debug_value {{.*}}, let, name "v", {{.*}}, scope [[GL_V:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "inner", {{.*}}, scope [[GL_INNER:[0-9]+]]
// CHECK: return {{.*}}, scope [[GL_V]]
public func guardLet(_ opt: Int?, _ flag: Bool) {
  guard let v = opt else { return }
  if flag {
    let inner = 42
    use(inner)
  }
}

// The loop exit is outside of the loop body.
// CHECK-LABEL: sil @$s1a8loopExityySaySiGF
// CHECK: debug_value {{.*}}, let, name "xs", argno 1, {{.*}}, scope [[LE_FN:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "inner", {{.*}}, scope [[LE_INNER:[0-9]+]]
// CHECK: return {{.*}}, scope [[LE_FN]]
public func loopExit(_ xs: [Int]) {
  for x in xs {
    let inner = x
    use(inner)
  }
}

// The cleanup of a binding in an enclosing block.
// CHECK-LABEL: sil @$s1a13nestedCleanupyySbF
// CHECK: debug_value [[NC_OUTER_VAL:%[0-9]+]], let, name "outer", {{.*}}, scope [[NC_OUTER:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "inner", {{.*}}, scope [[NC_INNER:[0-9]+]]
// CHECK: strong_release [[NC_OUTER_VAL]], {{.*}}, scope [[NC_OUTER]]
public func nestedCleanup(_ flag: Bool) {
  do {
    let outer = C()
    if flag {
      let inner = 42
      use(inner)
    }
  }
  use(flag)
}

// x's initializer has its own scope, a sibling of x's scope. The cleanup and
// the epilogue belong to x's scope.
// CHECK-LABEL: sil @$s1a20initializerThenBlockyyF
// CHECK: debug_value [[ITB_X_VAL:%[0-9]+]], let, name "x", {{.*}}, scope [[ITB_X:[0-9]+]]
// CHECK: debug_value {{.*}}, let, name "y", {{.*}}, scope [[ITB_Y:[0-9]+]]
// CHECK: strong_release [[ITB_X_VAL]], {{.*}}, scope [[ITB_X]]
// CHECK: return {{.*}}, scope [[ITB_X]]
public func initializerThenBlock() {
  let x = C()
  do {
    let y = 1
    use(y)
  }
}
