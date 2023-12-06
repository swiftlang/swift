// Verify basic support for DWARF Fission aka Split-DWARF
//
// RUN: %target-swift-frontend -c %/s -o /dev/null -g -debug-info-format=dwarf -split-dwarf-output %t_single.dwo
// RUN: %llvm-objdump -h %t_single.dwo | %FileCheck --check-prefix=SECTIONS %s
// RUN: %llvm-dwarfdump %t_single.dwo | %FileCheck --check-prefix=DWARF %s
//
// Check that section names have a .dwo suffix
// SECTIONS: .debug_info.dwo
//
// Check that there are actual DWARF entries
// DWARF: DW_TAG_compile_unit
// DWARF: fission.swift
// DWARF: DW_TAG_subprogram
// DWARF: foo

// Verify it works with multiple ins/outs
//
// RUN: %target-swift-frontend -c %/s %/S/Inputs/lib.swift -o /dev/null -g -debug-info-format=dwarf -split-dwarf-output %t_multi.dwo
// RUN: %llvm-dwarfdump %t_multi.dwo | %FileCheck --check-prefixes=DWARF,DWARF-MULTI %s
//
// Check that we get the extra DWARF entries
// DWARF-MULTI: DW_TAG_compile_unit
// DWARF-MULTI: lib.swift
// DWARF-MULTI: DW_TAG_subprogram
// DWARF-MULTI: libraryFunction

public func foo(_ a: Int64, _ b: Int64) -> Int64 {
    return a*b
}
