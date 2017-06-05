// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %api-digester -dump-sdk -module APINotesTest -o %t.dump1.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %S/Inputs/APINotesLeft
// RUN: %api-digester -dump-sdk -module APINotesTest -o %t.dump2.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %S/Inputs/APINotesRight
// RUN: %api-digester -compare-sdk --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result -json
// RUN: diff -u %S/Outputs/apinotes-migrator-gen.json %t.result
