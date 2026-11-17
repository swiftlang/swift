// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-sil -O -wmo -parse-as-library %t/Lib.swift -enforce-exclusivity=checked -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -module-name Lib -emit-module-path %t/Lib.swiftmodule -o %t/Lib.sil
// RUN: %FileCheck --check-prefix=LIB %s < %t/Lib.sil
// RUN: %target-swift-frontend -emit-sil -O -wmo -parse-as-library %t/Client.swift -enforce-exclusivity=checked -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -module-name Client -I %t | %FileCheck --check-prefix=CLIENT %s

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDynamicExclusivity

// Check that AccessEnforcementWMO doesn't miss accesses in serialized functions
// which are removed from the defining module after serialization. In Embedded
// Swift, generic functions are only serialized but not emitted in their
// defining module.
// rdar://191708388

//--- Lib.swift

private var conflicting = 0
private var nonConflicting = 0

public final class C {
  private var conflicting = 0
  public init() {}

  @inline(never) func modify(_ v: inout Int, _ f: () -> ()) { v += 1; f() }

  // Not emitted in Lib, but can be inlined into the client.
  public func withConflicting<T>(_ t: T, _ f: () -> ()) { modify(&conflicting, f) }

  // LIB-LABEL: sil [noinline] @$e3Lib1CC15readConflictingSiyF :
  // LIB:         begin_access [read] [dynamic] [no_nested_conflict]
  // LIB:       } // end sil function '$e3Lib1CC15readConflictingSiyF'
  @inline(never) public func readConflicting() -> Int { conflicting }
}

@inline(never) func modify(_ v: inout Int, _ f: () -> ()) { v += 1; f() }

// Not emitted in Lib, but can be inlined into the client.
public func withConflicting<T>(_ t: T, _ f: () -> ()) { modify(&conflicting, f) }

// LIB-LABEL: sil [noinline] @$e3Lib15readConflictingSiyF :
// LIB:         begin_access [read] [dynamic] [no_nested_conflict]
// LIB:       } // end sil function '$e3Lib15readConflictingSiyF'
@inline(never) public func readConflicting() -> Int { conflicting }

public func bumpNonConflicting<T>(_ t: T) { nonConflicting += 1 }

// LIB-LABEL: sil [noinline] @$e3Lib18readNonConflictingSiyF :
// LIB:         begin_access [read] [static] [no_nested_conflict]
// LIB:       } // end sil function '$e3Lib18readNonConflictingSiyF'
@inline(never) public func readNonConflicting() -> Int { nonConflicting }

//--- Client.swift

import Lib

// The access to `nonConflicting` was made static in Lib before serialization,
// so the inlined access doesn't have a dynamic access marker.

// CLIENT-LABEL: sil {{.*}}@{{main|__main_argc_argv}} :
// CLIENT:         global_addr @$e3Lib11conflicting
// CLIENT-NEXT:    begin_access [modify] [dynamic]
// CLIENT:         ref_element_addr {{.*}} #C.conflicting
// CLIENT:         begin_access [modify] [dynamic]
// CLIENT:         global_addr @$e3Lib14nonConflicting
// CLIENT-NOT:     begin_access
// CLIENT:       } // end sil function '{{main|__main_argc_argv}}'
@main
struct Main {
  static func main() {
    withConflicting(1) { _ = readConflicting() }
    let c = C()
    c.withConflicting(1) { _ = c.readConflicting() }
    bumpNonConflicting(1)
    _ = readNonConflicting()
  }
}
