// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Explicit)
// RUN: %empty-directory(%t/Mods)
// RUN: %empty-directory(%t/OutDirect)
// RUN: %empty-directory(%t/OutXfrontend)
// RUN: split-file %s %t

// Build ExplicitDep in %t/Explicit (not on -I search path) and MainMod in %t/Mods.
// RUN: %target-swift-frontend -emit-module -module-name ExplicitDep -o %t/Explicit/ExplicitDep.swiftmodule %t/ExplicitDep.swift
// RUN: %target-swift-frontend -emit-module -module-name MainMod -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -o %t/Mods/MainMod.swiftmodule %t/MainMod.swift

// Verify direct module-loading flags (-swift-module-file= and -target-sdk-version).
// RUN: %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -target-sdk-version 15.0 -pretty-print -output-dir %t/OutDirect
// RUN: %FileCheck %s --input-file %t/OutDirect/MainMod.symbols.json

// Verify -Xfrontend unwrapping for both single-token and two-token frontend options.
// RUN: %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -Xfrontend -target-sdk-version -Xfrontend 15.0 -pretty-print -output-dir %t/OutXfrontend
// RUN: %FileCheck %s --input-file %t/OutXfrontend/MainMod.symbols.json

// Verify unknown -Xfrontend flags are still rejected.
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -unknown-frontend-option-xyz -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=INVALID

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

// CHECK: "name": "MainMod"
// CHECK: "precise": "s:7MainMod6HolderV3dep11ExplicitDep0F4TypeVvp"
// INVALID: error: unknown argument: '-unknown-frontend-option-xyz'
