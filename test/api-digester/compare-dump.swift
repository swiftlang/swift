// RUN: %empty-directory(%t.mod1)
// RUN: %empty-directory(%t.mod2)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %swift -emit-module -o %t.mod1/cake.swiftmodule %S/Inputs/cake_baseline/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -module-name cake
// RUN: %swift -emit-module -o %t.mod2/cake.swiftmodule %S/Inputs/cake_current/cake.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesRight %clang-importer-sdk-nosource -module-name cake
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -I %S/Inputs/APINotesLeft > %t.dump1.json
// RUN: %api-digester -dump-sdk -module cake -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod2 -I %S/Inputs/APINotesLeft > %t.dump2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result

// RUN: sed 's|/[*].*[*]/||g' %S/Outputs/Cake.txt | sed '/^\s*$/d' > %t.expected
// RUN: sed 's|/[*].*[*]/||g' %t.result | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp

// Compare color against an empty baseline
// RUN: %swift -emit-module -o %t.mod1/color.swiftmodule %S/Inputs/cake_baseline/color.swift -parse-as-library -enable-library-evolution -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -module-name color
// RUN: %api-digester -diagnose-sdk -o %t.result -empty-baseline -I %S/Inputs/APINotesLeft -I %t.mod1 %clang-importer-sdk-nosource -module color -abi
// RUN: sed 's|/[*].*[*]/||g' %S/Outputs/color_vs_empty.txt | sed '/^\s*$/d' > %t.expected
// RUN: sed 's|/[*].*[*]/||g' %t.result | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp

// Run another module API checking without -enable-library-evolution
// RUN: %swift -emit-module -o %t.mod1/color.swiftmodule %S/Inputs/cake_baseline/color.swift -parse-as-library -I %S/Inputs/APINotesLeft %clang-importer-sdk-nosource -module-name color
// RUN: %swift -emit-module -o %t.mod2/color.swiftmodule %S/Inputs/cake_current/color.swift -parse-as-library -I %S/Inputs/APINotesRight %clang-importer-sdk-nosource -module-name color
// RUN: %api-digester -dump-sdk -module color -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod1 -I %S/Inputs/APINotesLeft > %t.dump1.json
// RUN: %api-digester -dump-sdk -module color -o - -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %t.mod2 -I %S/Inputs/APINotesLeft > %t.dump2.json
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result

// RUN: sed 's|/[*].*[*]/||g' %S/Outputs/color.txt | sed '/^\s*$/d' > %t.expected
// RUN: sed 's|/[*].*[*]/||g' %t.result | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp
