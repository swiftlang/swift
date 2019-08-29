// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)

// The goal of this test to make sure flag -use-interface-for-module works.
// We first build .swiftinterface with -enable-library-evolution
// Secondly, we We first build .swiftmodule without -enable-library-evolution
// Using swift-api-digester to load via .swiftinterface and .swiftmodule should
// always give us some difference.

// Generate .swiftinterface file for module cake
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.mod1/cake.swiftinterface %S/Inputs/cake_baseline/cake.swift -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -parse-as-library -enable-library-evolution -disable-objc-attr-requires-foundation-module -module-cache-path %t.module-cache

// Generate .swiftmodule file for module cake
// RUN: %target-swift-frontend -emit-module -o %t.mod1/cake.swiftmodule %S/Inputs/cake_baseline/cake.swift -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -parse-as-library  -disable-objc-attr-requires-foundation-module -module-cache-path %t.module-cache

// Dump Json file for cake ABI via .swiftmodule file
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -I %S/Inputs/APINotesLeft -abi > %t.dump1.json

// Dump Json file for cake ABI via .swiftinteface file
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -I %S/Inputs/APINotesLeft -abi -use-interface-for-module cake > %t.dump2.json

// Compare two Json files and we should see some differences.
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -abi -o %t.result

// RUN: %clang -E -P -x c %S/Outputs/Cake-binary-vs-interface.txt -o - | sed '/^\s*$/d' > %t.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp
