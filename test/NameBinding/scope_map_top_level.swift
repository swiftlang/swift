// Note: test of the scope map. All of these tests are line- and
// column-sensitive, so any additions should go at the end.

struct S0 { }

let a: Int? = 1

guard let b = a else {
}

func foo() {} // to interrupt the TopLevelCodeDecl

let c = b

typealias T = Int

extension Int {
  func my_identity() -> Int { return self }
}

var i: Int = b.my_identity()


// RUN: %target-swift-frontend -dump-scope-maps expanded %s 2> %t.expanded
// RUN: %FileCheck -check-prefix CHECK-EXPANDED %s < %t.expanded


// CHECK-EXPANDED:      ***Complete scope map***
// CHECK-EXPANDED-NEXT: ASTSourceFileScope {{.*}} '{{.*}}' [1:1 - 6{{[0-9]}}:1]
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}} 'S0' [4:1 - 4:13]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}} 'S0' [4:11 - 4:13]
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}} [6:1 - 21:28]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}} [6:1 - 21:28]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}} entry 0 [6:5 - 21:28]
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}} entry 0 [6:15 - 6:15]
// CHECK-EXPANDED-NEXT:       `-TopLevelCodeScope {{.*}} [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:         `-BraceStmtScope {{.*}} [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:           `-GuardStmtScope {{.*}} [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:             |-GuardConditionalClauseScope {{.*}} index 0 [8:7 - 8:22]
// CHECK-EXPANDED-NEXT:               `-StatementConditionElementPatternScope {{.*}}let b? [8:7 - 8:11]
// CHECK-EXPANDED-NEXT:             |-BraceStmtScope {{.*}} [8:22 - 9:1]
// CHECK-EXPANDED-NEXT:             `-GuardContinuationScope {{.*}} index 0 [9:1 - 21:28]
// CHECK-EXPANDED-NEXT:               |-AbstractFunctionDeclScope {{.*}} 'foo()' [11:1 - 11:13]
// CHECK-EXPANDED-NEXT:                 `-AbstractFunctionParamsScope {{.*}} [11:9 - 11:13]
// CHECK-EXPANDED-NEXT:                   `-PureFunctionBodyScope {{.*}} [11:12 - 11:13]
// CHECK-EXPANDED-NEXT:                     `-BraceStmtScope {{.*}} [11:12 - 11:13]
// CHECK-EXPANDED-NEXT:               `-TopLevelCodeScope {{.*}} [13:1 - 21:28]
// CHECK-EXPANDED-NEXT:                 `-BraceStmtScope {{.*}} [13:1 - 21:28]
// CHECK-EXPANDED-NEXT:                   `-PatternEntryDeclScope {{.*}} entry 0 [13:5 - 21:28]
// CHECK-EXPANDED-NEXT:                     |-PatternEntryInitializerScope {{.*}} entry 0 [13:9 - 13:9]
// CHECK-EXPANDED-NEXT:                     |-TypeAliasDeclScope {{.*}} <no extended nominal?!> [15:1 - 15:15]
// CHECK-EXPANDED-NEXT:                     |-ExtensionDeclScope {{.*}} 'Int' [17:1 - 19:1]
// CHECK-EXPANDED-NEXT:                       `-ExtensionBodyScope {{.*}} 'Int' [17:15 - 19:1]
// CHECK-EXPANDED-NEXT:                         `-AbstractFunctionDeclScope {{.*}} 'my_identity()' [18:3 - 18:43]
// CHECK-EXPANDED-NEXT:                           `-AbstractFunctionParamsScope {{.*}} [18:19 - 18:43]
// CHECK-EXPANDED-NEXT:                             `-MethodBodyScope {{.*}} [18:29 - 18:43]
// CHECK-EXPANDED-NEXT:                               `-BraceStmtScope {{.*}} [18:29 - 18:43]
// CHECK-EXPANDED-NEXT:                     `-TopLevelCodeScope {{.*}} [21:1 - 21:28]
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}} [21:1 - 21:28]
// CHECK-EXPANDED-NEXT:                         `-PatternEntryDeclScope {{.*}} entry 0 [21:5 - 21:28]
// CHECK-EXPANDED-NEXT:                           `-PatternEntryInitializerScope {{.*}} entry 0 [21:14 - 21:28]
