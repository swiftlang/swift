// RUN: rm -rf %t.mod && mkdir -p %t.mod
// RUN: rm -rf %t.sdk && mkdir -p %t.sdk
// RUN: rm -rf %t.module-cache && mkdir -p %t.module-cache
// RUN: %swift -emit-module -o %t.mod/cake.swiftmodule %S/Inputs/cake.swift -parse-as-library
// RUN: %api-digester -dump-sdk -module cake -o %t.dump.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %t.mod
// RUN: diff -u %t.dump.json %S/Outputs/cake.json
// RUN: %api-digester -diagnose-sdk --input-paths %t.dump.json -input-paths %S/Outputs/cake.json
