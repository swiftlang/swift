// Verify fission support aka Split-DWARF

// RUN: %target-swift-frontend -emit-module -parse-as-library %/s -g -debug-info-format=dwarf -o /dev/null -split-dwarf-output=%t.dwo
// TODO:
//   Looks like we cannot run tests with `swift-frontend -frontend -c ...` directly?
//   The RUN line is transformed into:   `swift-frontend.exe -target x86_64-unknown-windows-msvc ...`
//   And it fails with:
//   <unknown>:0: error: cannot open file 'S:/b/1/tools/swift\stdlib\windows-vfs-overlay.yaml' (no such file or directory)
//
//   `ninja check-swift` fails with:
//   error: 'tools/swift/test/swiftStdlibCollectionUnittest-windows', needed by 'tools/swift/test/CMakeFiles/check-swift-windows-x86_64', missing and no known rule to make it
//
// RUN: %llvm-objdump -h %t.dwo | %FileCheck --check-prefix=SECTIONS %s
// RUN: %llvm-dwarfdump %t.dwo | %FileCheck --check-prefix=DWARF %s

// Check that section names have a .dwo suffix
// SECTIONS: .debug_info.dwo

// Check that there are actual DWARF entries
// DWARF: DW_TAG_compile_unit
// DWARF: fission.swift
// DWARF: DW_TAG_subprogram
// DWARF: foo

public func foo(_ a: Int64, _ b: Int64) -> Int64 {
    return a*b
}
