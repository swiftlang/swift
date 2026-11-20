// RUN: %target-swift-frontend -emit-sil -O -wmo -parse-as-library %s -enforce-exclusivity=checked -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -module-name test | %FileCheck %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDynamicExclusivity

// Check that AccessEnforcementWMO removes dynamic exclusivity checks for non-public
// globals in Embedded Swift, even though such globals get public SIL linkage.
// rdar://191708388

private var privateCounter = 0
var internalCounter = 0
public var publicCounter = 0
private var conflictingCounter = 0

// CHECK-LABEL: sil [noinline] @$e4test11bumpPrivateyyF :
// CHECK:         begin_access [modify] [static] [no_nested_conflict]
// CHECK:       } // end sil function '$e4test11bumpPrivateyyF'
@inline(never) public func bumpPrivate() { privateCounter += 1 }

// CHECK-LABEL: sil [noinline] @$e4test11readPrivateSiyF :
// CHECK:         begin_access [read] [static] [no_nested_conflict]
// CHECK:       } // end sil function '$e4test11readPrivateSiyF'
@inline(never) public func readPrivate() -> Int { privateCounter }

// CHECK-LABEL: sil [noinline] @$e4test12bumpInternalyyF :
// CHECK:         begin_access [modify] [static] [no_nested_conflict]
// CHECK:       } // end sil function '$e4test12bumpInternalyyF'
@inline(never) public func bumpInternal() { internalCounter += 1 }

// A public global can be accessed from other modules.
// CHECK-LABEL: sil [noinline] @$e4test10bumpPublicyyF :
// CHECK:         begin_access [modify] [dynamic] [no_nested_conflict]
// CHECK:       } // end sil function '$e4test10bumpPublicyyF'
@inline(never) public func bumpPublic() { publicCounter += 1 }

// The access in `modifyConflicting` may have a nested conflict.
@inline(never) func modify(_ x: inout Int, _ body: () -> ()) {
  x += 1
  body()
}

// CHECK-LABEL: sil [noinline] @$e4test17modifyConflictingyyyyXEF :
// CHECK:         begin_access [modify] [dynamic]
// CHECK:       } // end sil function '$e4test17modifyConflictingyyyyXEF'
@inline(never) public func modifyConflicting(_ body: () -> ()) {
  modify(&conflictingCounter, body)
}

// CHECK-LABEL: sil [noinline] @$e4test15readConflictingSiyF :
// CHECK:         begin_access [read] [dynamic] [no_nested_conflict]
// CHECK:       } // end sil function '$e4test15readConflictingSiyF'
@inline(never) public func readConflicting() -> Int { conflictingCounter }

@main
struct Main {
  static func main() {
    bumpPrivate()
    _ = readPrivate()
    bumpInternal()
    bumpPublic()
    modifyConflicting { _ = readConflicting() }
  }
}
