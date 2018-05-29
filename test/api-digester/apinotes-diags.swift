// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %api-digester %clang-importer-sdk-nosource -dump-sdk -module APINotesTest -o %t.dump1.json -module-cache-path %t.module-cache -swift-version 4 -I %S/Inputs/APINotesLeft
// RUN: %api-digester %clang-importer-sdk-nosource -dump-sdk -module APINotesTest -o %t.dump2.json -module-cache-path %t.module-cache -swift-version 3 -I %S/Inputs/APINotesRight
// RUN: %api-digester %clang-importer-sdk-nosource -dump-sdk -module APINotesTest -o %t.dump3.json -module-cache-path %t.module-cache -swift-version 4 -I %S/Inputs/APINotesRight
// RUN: %api-digester -diagnose-sdk -print-module -input-paths %t.dump1.json -input-paths %t.dump3.json > %t.result
// RUN: diff -u %S/Outputs/apinotes-diags.txt %t.result
// RUN: %api-digester -diagnose-sdk -print-module -input-paths %t.dump2.json -input-paths %t.dump3.json > %t.result
// RUN: diff -u %S/Outputs/apinotes-diags-3-4.txt %t.result
