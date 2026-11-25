// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Explicit)
// RUN: %empty-directory(%t/Mods)
// RUN: %empty-directory(%t/OutDirect)
// RUN: %empty-directory(%t/OutXfrontend)
// RUN: %empty-directory(%t/OutInterleaved)
// RUN: split-file %s %t

// Build ExplicitDep in %t/Explicit (not on -I search path) and MainMod in %t/Mods.
// RUN: %target-swift-frontend -emit-module -module-name ExplicitDep -o %t/Explicit/ExplicitDep.swiftmodule %t/ExplicitDep.swift
// RUN: %target-swift-frontend -emit-module -module-name MainMod -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -o %t/Mods/MainMod.swiftmodule %t/MainMod.swift

// Verify direct frontend flags (-swift-module-file=, -target-sdk-version, diagnostic flags, and compile-only frontend flags).
// RUN: %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -target-sdk-version 15.0 -warnings-as-errors -diagnostic-style llvm -O -g -warn-long-function-bodies 100 -pretty-print -output-dir %t/OutDirect
// RUN: %FileCheck %s --input-file %t/OutDirect/MainMod.symbols.json

// Verify -Xfrontend unwrapping for single-token, two-token, and compile-only frontend options.
// RUN: %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -Xfrontend -target-sdk-version -Xfrontend 15.0 -Xfrontend -warn-long-function-bodies -Xfrontend 100 -pretty-print -output-dir %t/OutXfrontend
// RUN: %FileCheck %s --input-file %t/OutXfrontend/MainMod.symbols.json

// Verify interleaved multi-token -Xfrontend options mixed with direct driver tool options.
// RUN: %target-swift-symbolgraph-extract -Xfrontend -target-sdk-version -module-name MainMod -Xfrontend 15.0 -I %t/Mods -Xfrontend -swift-module-file=ExplicitDep=%t/Explicit/ExplicitDep.swiftmodule -pretty-print -output-dir %t/OutInterleaved
// RUN: %FileCheck %s --input-file %t/OutInterleaved/MainMod.symbols.json

// Verify diagnostic options are parsed and validated.
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -diagnostic-style invalid -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=BAD-DIAG

// Verify frontend action flags are rejected both directly and via -Xfrontend.
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -emit-ir -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=REJECTED-MODE
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -emit-ir -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=REJECTED-MODE
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -version -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=UNSUPPORTED-ACTION

// Verify unknown and non-frontend -Xfrontend flags and stray input files are rejected.
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -unknown-frontend-option-xyz -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=INVALID
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods -Xfrontend -pretty-print -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=NON-FRONTEND
// RUN: not %target-swift-symbolgraph-extract -module-name MainMod -I %t/Mods %t/MainMod.swift -output-dir %t/OutXfrontend 2>&1 | %FileCheck %s --check-prefix=STRAY-INPUT

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
// BAD-DIAG: error: unsupported argument 'invalid' to option '-diagnostic-style'
// REJECTED-MODE: error: this mode requires at least one input file
// UNSUPPORTED-ACTION: error: frontend action flags are not supported by this tool
// INVALID: error: unknown argument: '-unknown-frontend-option-xyz'
// NON-FRONTEND: error: unknown argument: '-pretty-print'
// STRAY-INPUT: error: unexpected input file '{{.*}}MainMod.swift'
