// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %t.mod
// RUN: diff -u %t.dump.json %S/Outputs/cake.json
// RUN: %api-digester -diagnose-sdk --input-paths %t.dump.json -input-paths %S/Outputs/cake.json
