// Without -dwarf-version, the frontend emits DWARF 5 for every target. The
// driver passes each target's default explicitly.

// DEFINE: %{emit-ir} = %swift -parse-stdlib -disable-legacy-type-info -emit-ir -g %s -o -

// RUN: %{emit-ir} -target arm64-apple-macosx13.0 | %FileCheck %s --check-prefix=DWARF5
// RUN: %{emit-ir} -target arm64-apple-none-macho | %FileCheck %s --check-prefix=DWARF5
// RUN: %{emit-ir} -target aarch64-unknown-linux-gnu | %FileCheck %s --check-prefix=DWARF5
// RUN: %{emit-ir} -target arm64-apple-macosx15.0 -dwarf-version=4 | %FileCheck %s --check-prefix=DWARF4

// REQUIRES: CODEGENERATOR=AArch64

public func f() {}

// DWARF4: !{i32 7, !"Dwarf Version", i32 4}
// DWARF5: !{i32 7, !"Dwarf Version", i32 5}
