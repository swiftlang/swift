// End-to-end test for the Swift driver in build system mode. A single swiftc
// invocation generates a temporary bridging PCH, compiles this file against it,
// links the executable, and runs dsymutil.
//
// The Clang types from the bridging header live in the temporary .pch, the
// object file's Clang skeleton unit in the object file points to the
// .pch. dsymutil has to open that .pch to copy the types into the dSYM bundle.

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -import-objc-header %S/Inputs/BridgingHeader.h \
// RUN:   -o %t/main %s 2> %t/build.err
// RUN: %FileCheck %s --check-prefix=BUILD --allow-empty --input-file=%t/build.err
// RUN: %llvm-dwarfdump --debug-info %t/main.dSYM | %FileCheck %s --check-prefix=DUMP

// dsymutil must be able to find the temporary bridging PCH; if the driver
// removed it before dsymutil ran, dsymutil would emit these diagnostics.
// BUILD-NOT: .pch: No such file or directory
// BUILD-NOT: while processing {{.*}}.pch

// 'Point' is declared in the bridging header, so its debug info only exists in
// the temporary PCH's split debug info. Check that dsymutil copied it into the
// dSYM.
// DUMP: DW_TAG_structure_type
// DUMP:   DW_AT_name ("Point")
// DUMP-NOT: DW_TAG
// DUMP:   DW_AT_decl_file ({{.*}}BridgingHeader.h")

let p = Point(x: 1, y: 2)
