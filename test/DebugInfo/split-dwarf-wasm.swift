// RUN: %empty-directory(%t)
// RUN: cd %t && %target-swiftc_driver -c %s \
// RUN:   -g -enable-split-dwarf -parse-stdlib -module-name test
// RUN: %llvm-dwarfdump -v --debug-info %t/split-dwarf-wasm.o | %FileCheck --check-prefix=OBJ %s
// RUN: %llvm-dwarfdump -v %t/split-dwarf-wasm.dwo | %FileCheck --check-prefix=DWO %s
// RUN: llvm-objdump -h %t/split-dwarf-wasm.dwo | %FileCheck --check-prefix=DWO-SECTIONS %s

// REQUIRES: CPU=wasm32

/// The object should contain only a skeleton CU (no full debug info).
// OBJ: DW_TAG_compile_unit
// OBJ:   DW_AT_GNU_dwo_name{{.*}}"split-dwarf-wasm.dwo"
// OBJ:   DW_AT_GNU_dwo_id
// OBJ-NOT: DW_TAG_subprogram

/// The DWO should contain the full debug info including subprograms.
// DWO: DW_TAG_compile_unit
// DWO: DW_TAG_subprogram

// DWO-SECTIONS: .debug_info.dwo
// DWO-SECTIONS: .debug_abbrev.dwo

public func hello() {}
