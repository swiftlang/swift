// LLDB needs reflection metadata to inspect variables, so -g combined with
// -disable-reflection-metadata should produce a warning.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -disable-reflection-metadata %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -emit-ir -o /dev/null -g -disable-reflection-metadata -warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=ERROR

// Line tables don't describe variables in the first place, so there is nothing
// to lose. -warnings-as-errors turns the absence of the warning into a hard
// failure, which an --allow-empty FileCheck alone would not catch.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -gline-tables-only -disable-reflection-metadata %s 2>&1 | %FileCheck %s --check-prefix=NO-WARNING --allow-empty
// RUN: %target-swift-frontend -emit-ir -o /dev/null -gline-tables-only -disable-reflection-metadata -warnings-as-errors %s
// Order doesn't matter, and neither does -gline-tables-only winning over -g.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -disable-reflection-metadata -gline-tables-only -warnings-as-errors %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -gline-tables-only -disable-reflection-metadata -warnings-as-errors %s
// ... but -g winning over -gline-tables-only does warn.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -gline-tables-only -g -disable-reflection-metadata %s 2>&1 | %FileCheck %s

// -gnone and no -g at all have no variables to inspect either.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -gnone -disable-reflection-metadata -warnings-as-errors %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -disable-reflection-metadata -warnings-as-errors %s

// -gdwarf-types describes variables in DWARF, so the debugger doesn't have to
// fall back onto reflection metadata.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -gdwarf-types -disable-reflection-metadata -warnings-as-errors %s

// The debugger still gets its reflection metadata here.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -disable-reflection-metadata -reflection-metadata-for-debugger-only -warnings-as-errors %s
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -reflection-metadata-for-debugger-only -warnings-as-errors %s

// Dropping only the reflection names keeps the metadata itself.
// RUN: %target-swift-frontend -emit-ir -o /dev/null -g -disable-reflection-names -warnings-as-errors %s

// CHECK: warning: debug info is requested with option '-g' but option '-disable-reflection-metadata' will prevent variable inspection in the debugger
// ERROR: error: debug info is requested with option '-g' but option '-disable-reflection-metadata' will prevent variable inspection in the debugger

// NO-WARNING-NOT: prevent variable inspection

func f() {
  let x = 42
  print(x)
}
