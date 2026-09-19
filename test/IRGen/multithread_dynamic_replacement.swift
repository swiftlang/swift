// Multi-threaded WMO: emit one LLVM module per source file. The dynamic
// replacements table (`l_unnamed_dynamic_replacements`) is emitted into the
// primary/first file's output module and references the replacement function
// with a *direct* (subtraction-based) relative pointer. The replacement
// function is declared in a *different* file, so under multi-threaded codegen
// it would be emitted into a different output module, making the relative
// reference cross an object-file boundary. That is unrepresentable on x86_64
// Mach-O (an undefined minuend in an X86_64_RELOC_SUBTRACTOR). The replacement
// function must therefore be co-located into the primary module alongside the
// table.
// rdar://187511655

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -O -parse-as-library -module-name Repro -wmo -num-threads 2 \
// RUN:   %t/a_first.swift %t/b.swift \
// RUN:   -o %t/a_first.ll -o %t/b.ll
// RUN: %FileCheck %s < %t/a_first.ll
// RUN: %FileCheck %s --check-prefix=OTHER < %t/b.ll

//--- a_first.swift
// This file is first, so it is the "primary" IRGen output module. The dynamic
// replacements table is emitted here, and the replacement function must be
// co-located into this module rather than left as an external reference to a
// definition in the other module.
public dynamic func target() -> Int { return 1 }

//--- b.swift
@_dynamicReplacement(for: target())
public func replacement() -> Int { return 2 }

// The replacements table lives in the primary module...
// CHECK: @"\01l_unnamed_dynamic_replacements" =
// ...and the replacement function is *defined* here, not an external
// declaration, so the table's direct relative reference stays within one object.
// CHECK: define {{.*}}@"$s5Repro11replacementSiyF"(
// CHECK-NOT: declare {{.*}}@"$s5Repro11replacementSiyF"(

// The replacement function must NOT be emitted into the other module.
// OTHER-NOT: define {{.*}}@"$s5Repro11replacementSiyF"(
