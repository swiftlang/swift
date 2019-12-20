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
// CHECK-EXPANDED-NEXT: ASTSourceFileScope {{.*}}, (uncached) [1:1 - 6{{.*}}:1] 'SOURCE_DIR{{[/\\]}}test{{[/\\]}}NameBinding{{[/\\]}}scope_map_top_level.swift'
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [4:1 - 4:13]
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [4:11 - 4:13]
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}}, [6:1 - 21:28]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}}, [6:1 - 21:28]
// CHECK-EXPANDED-NEXT:     |-PatternEntryDeclScope {{.*}}, [6:5 - 6:15] entry 0 'a'
// CHECK-EXPANDED-NEXT:       `-PatternEntryInitializerScope {{.*}}, [6:15 - 6:15] entry 0 'a'
// CHECK-EXPANDED-NEXT:     `-TopLevelCodeScope {{.*}}, [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:       `-BraceStmtScope {{.*}}, [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:         `-GuardStmtScope {{.*}}, [8:1 - 21:28]
// CHECK-EXPANDED-NEXT:           |-ConditionalClauseScope, [8:7 - 8:22] index 0
// CHECK-EXPANDED-NEXT:             `-ConditionalClausePatternUseScope, [8:22 - 8:22] let b{{\??}}
// CHECK-EXPANDED-NEXT:           |-BraceStmtScope {{.*}}, [8:22 - 9:1]
// CHECK-EXPANDED-NEXT:           `-LookupParentDiversionScope, [9:1 - 21:28]
// CHECK-EXPANDED-NEXT:             |-AbstractFunctionDeclScope {{.*}}, [11:1 - 11:13] 'foo()'
// CHECK-EXPANDED-NEXT:               `-ParameterListScope {{.*}}, [11:9 - 11:13]
// CHECK-EXPANDED-NEXT:                 `-PureFunctionBodyScope {{.*}}, [11:12 - 11:13]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [11:12 - 11:13]
// CHECK-EXPANDED-NEXT:             `-TopLevelCodeScope {{.*}}, [13:1 - 21:28]
// CHECK-EXPANDED-NEXT:               `-BraceStmtScope {{.*}}, [13:1 - 21:28]
// CHECK-EXPANDED-NEXT:                 |-PatternEntryDeclScope {{.*}}, [13:5 - 13:9] entry 0 'c'
// CHECK-EXPANDED-NEXT:                   `-PatternEntryInitializerScope {{.*}}, [13:9 - 13:9] entry 0 'c'
// CHECK-EXPANDED-NEXT:                 |-TypeAliasDeclScope {{.*}}, [15:1 - 15:15]
// CHECK-EXPANDED-NEXT:                 |-ExtensionDeclScope {{.*}}, [17:14 - 19:1]
// CHECK-EXPANDED-NEXT:                   `-ExtensionBodyScope {{.*}}, [17:15 - 19:1]
// CHECK-EXPANDED-NEXT:                     `-AbstractFunctionDeclScope {{.*}}, [18:3 - 18:43] 'my_identity()'
// CHECK-EXPANDED-NEXT:                       `-ParameterListScope {{.*}}, [18:19 - 18:43]
// CHECK-EXPANDED-NEXT:                         `-MethodBodyScope {{.*}}, [18:29 - 18:43]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}}, [18:29 - 18:43]
// CHECK-EXPANDED-NEXT:                 `-TopLevelCodeScope {{.*}}, [21:1 - 21:28]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [21:1 - 21:28]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}}, [21:5 - 21:28] entry 0 'i'
// CHECK-EXPANDED-NEXT:                       `-PatternEntryInitializerScope {{.*}}, [21:14 - 21:28] entry 0 'i'


// REQUIRES: asserts
// absence of assertions can change the "uncached" bit and cause failures
