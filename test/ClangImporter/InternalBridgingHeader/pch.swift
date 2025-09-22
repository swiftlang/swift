// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/tmp

// First test the explicit frontend-based bridging PCH generation and use works
// RUN: %target-swift-frontend -emit-pch -o %t/c-bridging-header.pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %t/c-bridging-header.pch -sdk %clang-importer-sdk

// Now test the driver-automated version is inert when disabled
// Output path of the PCH differs in the new driver, so force SWIFT_USE_OLD_DRIVER for now.
// RUN: env TMPDIR=%t/tmp/ SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -typecheck -disable-bridging-pch -save-temps %s -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: not ls %t/tmp/*.pch >/dev/null 2>&1

// Test the driver-automated version works by default
// Output path of the PCH differs in the new driver, so force SWIFT_USE_OLD_DRIVER for now.
// RUN: env TMPDIR=%t/tmp/ SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -typecheck -save-temps %s -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: ls %t/tmp/*.pch >/dev/null 2>&1
// RUN: llvm-objdump --raw-clang-ast %t/tmp/*.pch | llvm-bcanalyzer -dump | %FileCheck %s
// CHECK: ORIGINAL_FILE{{.*}}Inputs/c-bridging-header.h

// Test the driver-automated version deletes its PCH file when done
// RUN: rm %t/tmp/*.pch
// RUN: env TMPDIR=%t/tmp/ %target-swiftc_driver -typecheck %s -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: not ls %t/tmp/*.pch >/dev/null 2>&1

// Test -emit-pch invocation but with a persistent PCH
// RUN: %target-swift-frontend -emit-pch -pch-output-dir %t/pch %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -pch-output-dir %t/pch -pch-disable-validation -sdk %clang-importer-sdk
// RUN: ls %t/pch/*.pch >/dev/null 2>&1

// Test implicit use of persistent PCH
// RUN: %target-typecheck-verify-swift -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -pch-output-dir %t/pch2 -sdk %clang-importer-sdk
// RUN: ls %t/pch2/*.pch >/dev/null 2>&1

// RUN: touch %t/header.with.dot.h
// RUN: touch %t/test.swift
// RUN: %target-swift-frontend -typecheck %t/test.swift -internal-import-bridging-header %t/header.with.dot.h -pch-output-dir %t/pch_with_dot -module-cache-path %t/mcp1 -sdk %clang-importer-sdk
// RUN: %target-swift-frontend -typecheck %t/test.swift -internal-import-bridging-header %t/header.with.dot.h -pch-output-dir %t/pch_with_dot -module-cache-path %t/mcp2 -sdk %clang-importer-sdk
// RUN: ls %t/pch_with_dot/*swift*clang*.pch | count 2

// Test the driver-automated version using persistent PCH
// RUN: %target-swiftc_driver -typecheck -save-temps %s -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -pch-output-dir %t/pch3 -sdk %clang-importer-sdk
// RUN: ls %t/pch3/*.pch >/dev/null 2>&1
// RUN: llvm-objdump --raw-clang-ast %t/pch3/*.pch | llvm-bcanalyzer -dump | %FileCheck %s -check-prefix=PERSISTENT
// PERSISTENT: ORIGINAL_FILE{{.*}}Inputs/c-bridging-header.h

// Test that -pch-disable-validation works in that it won't implicitly create a PCH
// RUN: not %target-swift-frontend -typecheck %s -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -pch-output-dir %t/no-pch -pch-disable-validation -sdk %clang-importer-sdk 2>&1 | %FileCheck %s -check-prefix=NO-VALIDATION
// NO-VALIDATION: PCH file {{.*}} not found

// UNSUPPORTED: OS=windows-msvc

func getX(point: MyPoint) -> Double { point.x }
