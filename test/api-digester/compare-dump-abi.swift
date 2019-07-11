// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod1/cake.swiftmodule %S/Inputs/cake_baseline/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesLeft %clang-importer-sdk
// RUN: %swift -emit-module -o %t.mod2/cake.swiftmodule %S/Inputs/cake_current/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesRight %clang-importer-sdk
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk -I %t.mod1 -I %S/Inputs/APINotesLeft -abi > %t.dump1.json
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk -I %t.mod2 -I %S/Inputs/APINotesLeft -abi > %t.dump2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -abi -o %t.result
// RUN: %clang -E -P -x c %S/Outputs/Cake-abi.txt -o - | sed '/^\s*$/d' > %t.abi.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.abi.result.tmp
// RUN: diff -u %t.abi.expected %t.abi.result.tmp