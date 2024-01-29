// Verify basic support for Split-DWARF (1-to-1 in/out)
//
// RUN: %target-swift-frontend -c %/s -o %t_single.o -g -debug-info-format=dwarf -split-dwarf-path %t_single.dwo
// RUN: %llvm-dwarfdump -a %t_single.o | %FileCheck --check-prefix=OBJ -DDWO_PATH=%t_single.dwo %s
// RUN: %llvm-dwarfdump -a %t_single.dwo | %FileCheck --check-prefixes=DWO,DIE-FOO %s
//
// Check object file:
// * skeleton .debug_info and .debug_abbrev sections
//   OBJ: .debug_abbrev
//   OBJ: .debug_info
// * single unit with attributes for DWARF-object's file name and 64-bit ID
//   OBJ: DW_TAG_compile_unit
//   OBJ-NOT: DW_TAG_compile_unit
//   OBJ: DW_AT_GNU_dwo_name
//   COM: DW_AT_GNU_dwo_name ([[DWO_PATH]])   <-- Fails on Windows due to escaping for backslash path separators
//   OBJ: DW_AT_GNU_dwo_id (0x{{[0-9a-f]+}})
//
// Check DWARF-object file:
// * section names have a .dwo suffix
//   DWO: .debug_abbrev.dwo
//   DWO: .debug_info.dwo
// * there is a DIE for our function
//   DIE-FOO: DW_TAG_subprogram
//   DIE-FOO: DW_AT_name ("foo")

// Verify it works with a secondary input (N-to-1 in/out)
//
// RUN: %target-swift-frontend -c %/s %/S/Inputs/secondary.swift -o /dev/null -g -debug-info-format=dwarf -split-dwarf-path %t_multi.dwo
// RUN: %llvm-dwarfdump %t_multi.dwo | %FileCheck --check-prefixes=DIE-FOO,DIE-BAR %s
//
// Check the extra entry for the secondary input
// DIE-BAR: DW_TAG_subprogram
// DIE-BAR: DW_AT_name ("bar")

public func foo(_ a: Int64, _ b: Int64) -> Int64 {
    return a*b
}
