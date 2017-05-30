// REQUIRES: OS=macosx
// RUN: rm -rf %t.mod && mkdir -p %t.mod
// RUN: rm -rf %t.sdk && mkdir -p %t.sdk
// RUN: rm -rf %t.module-cache && mkdir -p %t.module-cache
// RUN: %api-digester -dump-sdk -module APINotesTest -o %t.dump1.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %S/Inputs/APINotesLeft
// RUN: %api-digester -dump-sdk -module APINotesTest -o %t.dump2.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %S/Inputs/APINotesRight
// RUN: %api-digester -compare-sdk --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result -json
// RUN: diff -u %S/Outputs/apinotes-migrator-gen.json %t.result
