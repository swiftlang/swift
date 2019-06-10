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
// CHECK-EXPANDED-NEXT: |-NominalTypeDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:4:1 - line:4:13]   'S0'
// CHECK-EXPANDED-NEXT:   `-NominalTypeBodyScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:4:11 - line:4:13]   'S0'
// CHECK-EXPANDED-NEXT: `-TopLevelCodeScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:6:1 - line:21:28]
// CHECK-EXPANDED-NEXT:   `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:6:1 - line:21:28]
// CHECK-EXPANDED-NEXT:     `-PatternEntryDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:6:5 - line:21:28]   entry 0 'a'
// CHECK-EXPANDED-NEXT:       |-PatternEntryInitializerScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:6:15 - line:6:15]   entry 0 'a'
// CHECK-EXPANDED-NEXT:       `-PatternEntryUseScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:6:15 - line:21:28]   entry 0 'a'
// CHECK-EXPANDED-NEXT:         `-TopLevelCodeScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:1 - line:21:28]
// CHECK-EXPANDED-NEXT:           `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:1 - line:21:28]
// CHECK-EXPANDED-NEXT:             `-GuardStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:1 - line:21:28]
// CHECK-EXPANDED-NEXT:               |-ConditionalClauseScope, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:7 - line:8:22] {{.*}}}} index 0
// CHECK-EXPANDED-NEXT:                 `-ConditionalClausePatternUseScope, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:22 - line:8:22]  let b?
// CHECK-EXPANDED-NEXT:               |-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:8:22 - line:9:1]
// CHECK-EXPANDED-NEXT:               `-GuardStmtUseScope, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:9:1 - line:21:28]
// CHECK-EXPANDED-NEXT:                 |-AbstractFunctionDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:11:1 - line:11:13]   'foo()'
// CHECK-EXPANDED-NEXT:                   `-AbstractFunctionParamsScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:11:9 - line:11:13]
// CHECK-EXPANDED-NEXT:                     `-PureFunctionBodyScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:11:12 - line:11:13]
// CHECK-EXPANDED-NEXT:                       `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:11:12 - line:11:13]
// CHECK-EXPANDED-NEXT:                 `-TopLevelCodeScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:13:1 - line:21:28]
// CHECK-EXPANDED-NEXT:                   `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:13:1 - line:21:28]
// CHECK-EXPANDED-NEXT:                     `-PatternEntryDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:13:5 - line:21:28]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                       |-PatternEntryInitializerScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:13:9 - line:13:9]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                       `-PatternEntryUseScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:13:9 - line:21:28]   entry 0 'c'
// CHECK-EXPANDED-NEXT:                         |-TypeAliasDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:15:1 - line:15:15]   <no extended nominal?!>
// CHECK-EXPANDED-NEXT:                         |-ExtensionDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:17:1 - line:19:1]   'Int'
// CHECK-EXPANDED-NEXT:                           `-ExtensionBodyScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:17:15 - line:19:1]   'Int'
// CHECK-EXPANDED-NEXT:                             `-AbstractFunctionDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:18:3 - line:18:43]   'my_identity()'
// CHECK-EXPANDED-NEXT:                               `-AbstractFunctionParamsScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:18:19 - line:18:43]
// CHECK-EXPANDED-NEXT:                                 `-MethodBodyScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:18:29 - line:18:43]
// CHECK-EXPANDED-NEXT:                                   `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:18:29 - line:18:43]
// CHECK-EXPANDED-NEXT:                         `-TopLevelCodeScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:21:1 - line:21:28]
// CHECK-EXPANDED-NEXT:                           `-BraceStmtScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:21:1 - line:21:28]
// CHECK-EXPANDED-NEXT:                             `-PatternEntryDeclScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:21:5 - line:21:28]   entry 0 'i'
// CHECK-EXPANDED-NEXT:                               |-PatternEntryInitializerScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:21:14 - line:21:28]   entry 0 'i'
// CHECK-EXPANDED-NEXT:                               `-PatternEntryUseScope {{.*}}, [/s/exp-dep/swift/test/NameBinding/scope_map_top_level.swift:21:28 - line:21:28]   entry 0 'i'

