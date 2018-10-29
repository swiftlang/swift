// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod/cake1.swiftmodule %S/Inputs/cake1.swift -parse-as-library -enable-resilience -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource
// RUN: %swift -emit-module -o %t.mod/cake2.swiftmodule %S/Inputs/cake2.swift -parse-as-library -enable-resilience -I %S/Inputs/APINotesRight %clang-importer-sdk-nosource
// RUN: %api-digester -dump-sdk -module cake1 -o %t.dump1.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod -I %S/Inputs/APINotesLeft
// RUN: %api-digester -dump-sdk -module cake2 -o %t.dump2.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod -I %S/Inputs/APINotesRight
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result

// RUN: %clang -E -P -x c %S/Outputs/Cake.txt -o - | sed '/^\s*$/d' > %t.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp

// RUN: %api-digester -dump-sdk -module cake1 -o %t.dump1.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod -I %S/Inputs/APINotesLeft -abi
// RUN: %api-digester -dump-sdk -module cake2 -o %t.dump2.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod -I %S/Inputs/APINotesRight -abi
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -abi -o %t.result

// RUN: %clang -E -P -x c %S/Outputs/Cake-abi.txt -o - | sed '/^\s*$/d' > %t.abi.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.abi.result.tmp
// RUN: diff -u %t.abi.expected %t.abi.result.tmp
