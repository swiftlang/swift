// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Explicit)
// RUN: %empty-directory(%t/Mods)
// RUN: split-file %s %t

// Build ExplicitDep in %t/Explicit (not on -I search path) and MainMod in %t/Mods.
// RUN: %target-swift-frontend -emit-module -module-name ExplicitDep -o %t/Explicit/ExplicitDep.swiftmodule %t/ExplicitDep.swift
// RUN: %target-swift-frontend -emit-module -module-name MainMod -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -o %t/Mods/MainMod.swiftmodule %t/MainMod.swift

// Verify direct module-loading flags (-swift-module-file= and -target-sdk-version).
// RUN: %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -target-sdk-version 15.0 -o - | %FileCheck %s

// Verify -Xfrontend unwrapping for both single-token and two-token frontend options.
// RUN: %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -Xfrontend -target-sdk-version -Xfrontend 15.0 -o - | %FileCheck %s

// Verify unknown -Xfrontend flags are still rejected.
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -unknown-frontend-option-xyz -o - 2>&1 | %FileCheck %s --check-prefix=INVALID

//--- ExplicitDep.swift
public struct DepType {
  public init() {}
}

//--- MainMod.swift
import ExplicitDep

public struct Holder {
  public var dep: DepType
  public init(dep: DepType) {
    self.dep = dep
  }
}

// CHECK: public struct Holder {
// CHECK:   public var dep: ExplicitDep.DepType
// CHECK:   public init(dep: ExplicitDep.DepType)
// CHECK: }
// INVALID: error: unknown argument: '-unknown-frontend-option-xyz'
