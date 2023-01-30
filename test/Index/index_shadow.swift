// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s --include-locals | %FileCheck %s

// The index will output references to the shadowed-declaration rather than
// the one defined by the shorthand if-let or capture. It also skips
// outputting the shadowing-definiiton since it would then have no references.
struct ShadowedTest {
  // CHECK: [[@LINE+1]]:7 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Def
  let shadowedVar: Int?? = 1

  func localShadowTest() {
    // CHECK: [[@LINE+1]]:9 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Def
    let localShadowedVar: Int? = 2

    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} localShadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
    if let localShadowedVar {
      // CHECK: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
      _ = localShadowedVar
    }

    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} localShadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
    _ = { [localShadowedVar] in
      // CHECK: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
      _ = localShadowedVar
    }
  }

  func shadowTest() {
    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} shadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    if let shadowedVar {
      // CHECK: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = shadowedVar

      // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedVar {{.*}}Def
      // CHECK: [[@LINE+1]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      if let shadowedVar {
        // CHECK: [[@LINE+1]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
        _ = shadowedVar
      }
    }

    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} shadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    _ = { [shadowedVar] in
      // CHECK: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = shadowedVar

      // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedVar {{.*}}Def
      // CHECK: [[@LINE+1]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = { [shadowedVar] in
        // CHECK: [[@LINE+1]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
        _ = shadowedVar
      }
    }
  }

  func nestedFuncTest() {
    // CHECK: [[@LINE+1]]:10 {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyF {{.*}}Def
    func shadowedFunc() {
      // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedFunc {{.*}}Def
      // CHECK: [[@LINE+1]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyF {{.*}}Ref
      _ = { [shadowedFunc] in
        // CHECK: [[@LINE+1]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyF {{.*}}Ref
        _ = shadowedFunc
      }
    }
  }
}
