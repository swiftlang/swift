// RUN: %target-swift-frontend -swift-version 4 -enforce-exclusivity=checked %s -emit-ir -module-name CurrentModule -D CURRENT_MODULE | %FileCheck %s --check-prefix=CHECK-SAMEMAJOR --check-prefix=CHECK-DIFFMAJOR
// REQUIRES: OS=macosx

@available(OSX 10.8, *)
@_originallyDefinedIn(module: "OriginalModule", macOS 10.10)
public struct Entity {
  public func addEntity(_ e: Entity) {}
  public func removeEntity(_ e: Entity) {}
}

// CHECK-SAMEMAJOR: $ld$hide$os10.8$_$s14OriginalModule6EntityVN
// CHECK-SAMEMAJOR: $ld$hide$os10.9$_$s14OriginalModule6EntityVN
// CHECK-SAMEMAJOR-NOT: $ld$hide$os10.10$_$s14OriginalModule6EntityVN

@available(OSX 9.8, *)
@_originallyDefinedIn(module: "OriginalModule", macOS 10.10)
public struct OldEntity {
  public func addEntity(_ e: Entity) {}
  public func removeEntity(_ e: Entity) {}
}

// CHECK-DIFFMAJOR: $ld$hide$os9.9$_$s14OriginalModule9OldEntityVN
// CHECK-DIFFMAJOR: $ld$hide$os9.13$_$s14OriginalModule9OldEntityVN
// CHECK-DIFFMAJOR: $ld$hide$os9.30$_$s14OriginalModule9OldEntityVN
// CHECK-DIFFMAJOR: $ld$hide$os10.8$_$s14OriginalModule9OldEntityVN
// CHECK-DIFFMAJOR: $ld$hide$os10.9$_$s14OriginalModule9OldEntityVN
// CHECK-DIFFMAJOR-NOT: $ld$hide$os10.10$_$s14OriginalModule9OldEntityVN
