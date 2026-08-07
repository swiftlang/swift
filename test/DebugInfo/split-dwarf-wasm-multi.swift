// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// Compile two files with split DWARF via the driver.
// RUN: %empty-directory(%t/out)
// RUN: cd %t/out && %target-swiftc_driver -c %t/main.swift %t/other.swift \
// RUN:   -g -enable-split-dwarf -parse-stdlib -module-name TestMod

// Verify each object has only a skeleton CU.
// RUN: %llvm-dwarfdump -v --debug-info %t/out/main.o | %FileCheck --check-prefix=MAIN-OBJ %s
// RUN: %llvm-dwarfdump -v --debug-info %t/out/other.o | %FileCheck --check-prefix=OTHER-OBJ %s

// Verify each .dwo has the full debug info.
// RUN: %llvm-dwarfdump -v --debug-info %t/out/main.dwo | %FileCheck --check-prefix=MAIN-DWO %s
// RUN: %llvm-dwarfdump -v --debug-info %t/out/other.dwo | %FileCheck --check-prefix=OTHER-DWO %s

// Merge into .dwp and verify both CUs are present.
// RUN: llvm-dwp %t/out/main.dwo %t/out/other.dwo -o %t/out/TestMod.dwp
// RUN: %llvm-dwarfdump -v --debug-info %t/out/TestMod.dwp | %FileCheck --check-prefix=MERGED %s

// REQUIRES: CPU=wasm32

// main.o: skeleton only, no subprogram
// MAIN-OBJ: DW_TAG_compile_unit
// MAIN-OBJ:   DW_AT_GNU_dwo_name{{.*}}"main.dwo"
// MAIN-OBJ:   DW_AT_GNU_dwo_id
// MAIN-OBJ-NOT: DW_TAG_subprogram

// other.o: skeleton only, no subprogram
// OTHER-OBJ: DW_TAG_compile_unit
// OTHER-OBJ:   DW_AT_GNU_dwo_name{{.*}}"other.dwo"
// OTHER-OBJ:   DW_AT_GNU_dwo_id
// OTHER-OBJ-NOT: DW_TAG_subprogram

// main.dwo: full debug info with foo()
// MAIN-DWO: DW_TAG_compile_unit
// MAIN-DWO: DW_TAG_subprogram
// MAIN-DWO:   DW_AT_name{{.*}}"foo"

// other.dwo: full debug info with bar()
// OTHER-DWO: DW_TAG_compile_unit
// OTHER-DWO: DW_TAG_subprogram
// OTHER-DWO:   DW_AT_name{{.*}}"bar"

// Merged .dwp: both CUs present (order matches input order to llvm-dwp)
// MERGED: DW_TAG_compile_unit
// MERGED:   DW_AT_name{{.*}}main.swift
// MERGED: DW_TAG_subprogram
// MERGED:   DW_AT_name{{.*}}"foo"
// MERGED: DW_TAG_compile_unit
// MERGED:   DW_AT_name{{.*}}other.swift
// MERGED: DW_TAG_subprogram
// MERGED:   DW_AT_name{{.*}}"bar"

// BEGIN main.swift
public func foo() {}

// BEGIN other.swift
public func bar() {}
