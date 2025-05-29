// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %empty-directory(%t.baseline/ABI)

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod1/cake.swiftmodule %S/Inputs/cake_baseline/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -emit-module-source-info -emit-module-source-info-path %t.mod1/cake.swiftsourceinfo 2> %t.compiler-diags
// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t.mod2/cake.swiftmodule %S/Inputs/cake_current/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesRight %clang-importer-sdk-nosource -emit-module-source-info -emit-module-source-info-path %t.mod2/cake.swiftsourceinfo
// RUN: %api-digester -dump-sdk -module cake -output-dir %t.baseline -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -I %S/Inputs/APINotesLeft -abi
// RUN: %api-digester -diagnose-sdk -print-module -baseline-dir %t.baseline -module cake -I %t.mod2 -I %S/Inputs/APINotesLeft -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -o %t.result
// RUN: %clang -E -P -w -x c %S/Outputs/Cake-abi.txt -o - | sed '/^\s*$/d' > %t.abi.expected
// RUN: %clang -E -P -w -x c %t.result -o - | sed '/^\s*$/d' > %t.abi.result.tmp
// RUN: diff -u %t.abi.expected %t.abi.result.tmp

// A compiler-style diag to ensure we have source locations associated with breakages.
// RUN: not %api-digester -diagnose-sdk -print-module -baseline-dir %t.baseline -module cake -I %t.mod2 -I %S/Inputs/APINotesLeft -module-cache-path %t.module-cache %clang-importer-sdk-nosource -abi -o %t.result -compiler-style-diags 2> %t.abi.compiler.diags
// RUN: %FileCheck %s < %t.abi.compiler.diags

// CHECK: cake_current/cake.swift:39:15: error: ABI breakage: struct C6 is now with @frozen
// CHECK: cake_current/cake.swift:41:13: error: ABI breakage: enum IceKind is now without @frozen



