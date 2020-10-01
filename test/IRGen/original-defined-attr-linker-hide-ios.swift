// RUN: %target-swift-frontend -swift-version 4 -enforce-exclusivity=checked %s -emit-ir -module-name CurrentModule -D CURRENT_MODULE | %FileCheck %s

// REQUIRES: OS=ios

@available(iOS 5.0, OSX 10.10, *)
@_originallyDefinedIn(module: "OriginalModule", iOS 5.4, OSX 10.13)
public struct Entity {
  public func addEntity(_ e: Entity) {}
  public func removeEntity(_ e: Entity) {}
}

// CHECK: $ld$hide$os5.0$_$s14OriginalModule6EntityVN
// CHECK: $ld$hide$os5.1$_$s14OriginalModule6EntityVN
// CHECK: $ld$hide$os5.2$_$s14OriginalModule6EntityVN
// CHECK: $ld$hide$os5.3$_$s14OriginalModule6EntityVN
// CHECK-NOT: $ld$hide$os5.4$_$s14OriginalModule6EntityVN
