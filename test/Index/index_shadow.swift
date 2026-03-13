// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s -include-locals | %FileCheck %s -check-prefix=CHECK_LOCALS

// The index will output references to the shadowed-declaration rather than
// the one defined by the shorthand if-let or capture. It also skips
// outputting the shadowing-definiiton since it would then have no references.
struct ShadowedTest {
  // CHECK: [[@LINE+1]]:7 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Def
  let shadowedVar: Int?? = 1

  func localShadowTest() {
    // CHECK_LOCALS: [[@LINE+1]]:9 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Def
    let localShadowedVar: Int? = 2

    // CHECK_LOCALS: [[@LINE+2]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
    // CHECK_LOCALS: [[@LINE+1]]:12 | variable(local)/Swift{{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL0_Sivp {{.*}}Def
    if let localShadowedVar {
      // CHECK_LOCALS-NOT: [[@LINE+2]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}
      // CHECK_LOCALS: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL0_Sivp {{.*}}Ref
      _ = localShadowedVar
    }

    // CHECK_LOCALS: [[@LINE+2]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}Ref
    // CHECK_LOCALS: [[@LINE+1]]:12 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL1_SiSgvp {{.*}}Def
    _ = { [localShadowedVar] in
      // CHECK_LOCALS-NOT: [[@LINE+2]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL_SiSgvp {{.*}}
      // CHECK_LOCALS: [[@LINE+1]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV011localShadowE0yyF0fD3VarL1_SiSgvp {{.*}}Ref
      _ = localShadowedVar
    }
  }

  func shadowTest() {
    // CHECK_LOCALS: [[@LINE+4]]:12 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL_SiSgvp {{.*}}Def
    // CHECK_LOCALS: [[@LINE+3]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} shadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    if let shadowedVar {
      // CHECK_LOCALS-NOT: [[@LINE+3]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      // CHECK_LOCALS: [[@LINE+2]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL_SiSgvp {{.*}}Ref
      // CHECK: [[@LINE+1]]:11 | instance-property/Swift | shadowedVar | s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = shadowedVar

      // CHECK_LOCALS: [[@LINE+4]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      // CHECK_LOCALS: [[@LINE+3]]:14 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL0_Sivp {{.*}}Def
      // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedVar {{.*}}Def
      // CHECK: [[@LINE+1]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      if let shadowedVar {
        // CHECK_LOCALS: [[@LINE+2]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL0_Sivp {{.*}}Ref
        // CHECK: [[@LINE+1]]:13 | instance-property/Swift | shadowedVar | s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
        _ = shadowedVar
      }
    }

    // CHECK_LOCALS: [[@LINE+4]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    // CHECK_LOCALS: [[@LINE+3]]:12 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL1_SiSgSgvp {{.*}}Def
    // CHECK-NOT: [[@LINE+2]]:12 {{.*}} shadowedVar {{.*}}Def
    // CHECK: [[@LINE+1]]:12 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
    _ = { [shadowedVar] in
      // CHECK_LOCALS: [[@LINE+2]]:11 {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyF11shadowedVarL1_SiSgSgvp {{.*}}Ref
      // CHECK: [[@LINE+1]]:11 | instance-property/Swift | shadowedVar | s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = shadowedVar

      // CHECK_LOCALS: [[@LINE+4]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      // CHECK_LOCALS: [[@LINE+3]]:14 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyFyycfU_11shadowedVarL_SiSgSgvp {{.*}}Def
      // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedVar {{.*}}Def
      // CHECK: [[@LINE+1]]:14 {{.*}} s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
      _ = { [shadowedVar] in
        // CHECK_LOCALS: [[@LINE+2]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV06shadowE0yyFyycfU_11shadowedVarL_SiSgSgvp {{.*}}Ref
        // CHECK: [[@LINE+1]]:13 | instance-property/Swift | shadowedVar | s:14swift_ide_test12ShadowedTestV11shadowedVarSiSgSgvp {{.*}}Ref
        _ = shadowedVar
      }
    }
  }

  func nestedFuncTest() {
    // CHECK_LOCALS: [[@LINE+1]]:10 {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyF {{.*}}Def
    func shadowedFunc() {
      // CHECK_LOCALS: [[@LINE+1]]:14 | variable(local)/Swift {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyFAEL_yycvp {{.*}}Def
      _ = { [shadowedFunc] in
        // CHECK-NOT: [[@LINE+2]]:14 {{.*}} shadowedFunc {{.*}}Def
        // CHECK_LOCALS: [[@LINE+1]]:13 {{.*}} s:14swift_ide_test12ShadowedTestV010nestedFuncE0yyF08shadowedG0L_yyFAEL_yycvp {{.*}}Ref
        _ = shadowedFunc
      }
    }
  }
}
