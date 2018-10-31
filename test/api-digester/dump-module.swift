// SWIFT_ENABLE_TENSORFLOW
// UNSUPPORTED: tensorflow
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: diff -u %S/Outputs/cake.json %t.dump.json
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -swift-only -I %t.mod -abi -I %S/Inputs/ClangCake %clang-importer-sdk-nosource
// RUN: diff -u %S/Outputs/cake-abi.json %t.dump.json
// RUN: %api-digester -diagnose-sdk --input-paths %t.dump.json -input-paths %S/Outputs/cake.json

// Round-trip testing:
// RUN: %api-digester -deserialize-sdk --input-paths %S/Outputs/cake.json -o %t.dump.json
// RUN: diff -u %S/Outputs/cake.json %t.dump.json
// RUN: %api-digester -deserialize-sdk --input-paths %S/Outputs/cake-abi.json -o %t.dump.json
// RUN: diff -u %S/Outputs/cake-abi.json %t.dump.json
