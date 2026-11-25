// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Explicit)
// RUN: %empty-directory(%t/Mods)
// RUN: split-file %s %t

// Build ExplicitDep in %t/Explicit (not on -I search path) and MainMod in %t/Mods.
// RUN: %target-swift-frontend -emit-module -module-name ExplicitDep -o %t/Explicit/ExplicitDep.swiftmodule %t/ExplicitDep.swift
// RUN: %target-swift-frontend -emit-module -module-name MainMod -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -o %t/Mods/MainMod.swiftmodule %t/MainMod.swift

// Verify direct frontend flags (-swift-module-file=, -target-sdk-version, diagnostic flags, compile-only flags, and -o <file> with no source inputs).
// RUN: %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -target-sdk-version 15.0 -warnings-as-errors -diagnostic-style llvm -O -g -warn-long-function-bodies 100 -o %t/MainMod.swiftinterface
// RUN: %FileCheck %s --input-file %t/MainMod.swiftinterface

// Verify -Xfrontend unwrapping for single-token, two-token, and compile-only frontend options.
// RUN: %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -Xfrontend -target-sdk-version -Xfrontend 15.0 -Xfrontend -warn-long-function-bodies -Xfrontend 100 -o - | %FileCheck %s

// Verify interleaved multi-token -Xfrontend options mixed with direct driver tool options.
// RUN: %target-swift-synthesize-interface -Xfrontend -target-sdk-version -module-name MainMod -Xfrontend 15.0 -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -o - | %FileCheck %s

// Verify diagnostic options are parsed and validated.
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -diagnostic-style invalid -o - 2>&1 | %FileCheck %s --check-prefix=BAD-DIAG

// Verify frontend action flags are rejected both directly and via -Xfrontend.
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -emit-ir -o - 2>&1 | %FileCheck %s --check-prefix=REJECTED-MODE
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -emit-ir -o - 2>&1 | %FileCheck %s --check-prefix=REJECTED-MODE
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -version -o - 2>&1 | %FileCheck %s --check-prefix=UNSUPPORTED-ACTION

// Verify unknown and non-frontend -Xfrontend flags and stray input files are rejected.
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -unknown-frontend-option-xyz -o - 2>&1 | %FileCheck %s --check-prefix=INVALID
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods -Xfrontend -v -o - 2>&1 | %FileCheck %s --check-prefix=NON-FRONTEND
// RUN: not %target-swift-synthesize-interface -module-name MainMod -I %t/Mods %t/MainMod.swift -o - 2>&1 | %FileCheck %s --check-prefix=STRAY-INPUT

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
// BAD-DIAG: error: unsupported argument 'invalid' to option '-diagnostic-style'
// REJECTED-MODE: error: this mode requires at least one input file
// UNSUPPORTED-ACTION: error: frontend action flags are not supported by this tool
// INVALID: error: unknown argument: '-unknown-frontend-option-xyz'
// NON-FRONTEND: error: unknown argument: '-v'
// STRAY-INPUT: error: unexpected input file '{{.*}}MainMod.swift'
