// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod/cake1.swiftmodule %S/Inputs/cake1.swift -parse-as-library -I %S/Inputs/APINotesLeft
// RUN: %swift -emit-module -o %t.mod/cake2.swiftmodule %S/Inputs/cake2.swift -parse-as-library -I %S/Inputs/APINotesRight
// RUN: %api-digester -dump-sdk -module cake1 -o %t.dump1.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %t.mod -I %S/Inputs/APINotesLeft
// RUN: %api-digester -dump-sdk -module cake2 -o %t.dump2.json -module-cache-path %t.module-cache -sdk %t.sdk -swift-version 3 -I %t.mod -I %S/Inputs/APINotesRight
// RUN: %api-digester -diagnose-sdk --input-paths %t.dump1.json -input-paths %t.dump2.json > %t.result
// RUN: diff -u %S/Outputs/Cake.txt %t.result
