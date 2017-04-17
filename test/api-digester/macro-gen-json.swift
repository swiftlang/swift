// RUN: rm -rf %t.mod && mkdir -p %t.mod
// RUN: rm -rf %t.sdk && mkdir -p %t.sdk
// RUN: rm -rf %t.module-cache && mkdir -p %t.module-cache
// RUN: %swift -emit-module -o %t.mod/macrogenleft.swiftmodule %S/Inputs/macro-gen-left.swift -parse-as-library
// RUN: %swift -emit-module -o %t.mod/macrogenright.swiftmodule %S/Inputs/macro-gen-right.swift -parse-as-library
// RUN: %api-digester -dump-sdk -module macrogenleft -o %t.dump1.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %t.mod
// RUN: %api-digester -dump-sdk -module macrogenright -o %t.dump2.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 4 -I %t.mod
// RUN: %api-digester -compare-sdk --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result -json
// RUN: diff -u %S/Outputs/macro-gen.json %t.result
