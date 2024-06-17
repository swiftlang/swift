// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.mod1/cake.swiftinterface %S/Inputs/cake_baseline/cake.swift -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -parse-as-library -enable-library-evolution -disable-objc-attr-requires-foundation-module -module-cache-path %t.module-cache
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.mod2/cake.swiftinterface %S/Inputs/cake_current/cake.swift -I %S/Inputs/APINotesRight %clang-importer-sdk-nosource -parse-as-library -enable-library-evolution -disable-objc-attr-requires-foundation-module -module-cache-path %t.module-cache
// RUN: %api-digester -diagnose-sdk -print-module -module cake -BI %t.mod1 -BI %S/Inputs/APINotesLeft -I %t.mod2 -I %S/Inputs/APINotesRight -sdk %clang-importer-sdk-path -bsdk %clang-importer-sdk-path -module-cache-path %t.module-cache -o %t.result

// RUN: %clang -E -P -x c %S/Outputs/Cake.txt -o - | sed '/^\s*$/d' > %t.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp



