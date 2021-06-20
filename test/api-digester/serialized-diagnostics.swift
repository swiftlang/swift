// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-module -o %t/color.swiftmodule %S/Inputs/cake_baseline/color.swift -parse-as-library -enable-library-evolution %clang-importer-sdk-nosource -module-name color
// RUN: not %api-digester -diagnose-sdk -serialize-diagnostics-path %t/result.dia -empty-baseline -I %t %clang-importer-sdk-nosource -module color -abi
// RUN: c-index-test -read-diagnostics %t/result.dia 2>&1 | %FileCheck %s -check-prefix CHECK-DIA
// RUN: %api-digester -diagnose-sdk -serialize-diagnostics-path %t/result.dia -empty-baseline -I %t %clang-importer-sdk-nosource -module color -abi -disable-fail-on-error
// RUN: c-index-test -read-diagnostics %t/result.dia 2>&1 | %FileCheck %s -check-prefix CHECK-DIA

// Ensure the 'api-digester-breaking-change' category is included in the serialized diagnostics file.
// CHECK-DIA: error: ABI breakage: enum Color is a new API without @available attribute [] [api-digester-breaking-change]
