// RUN: %empty-directory(%t.module-cache)
// RUN: %api-digester -dump-sdk -module Foo -o %t.dump1.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %S/Inputs/Foo -avoid-location
// RUN: %api-digester -dump-sdk -module Foo -o %t.dump2.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %S/Inputs/Foo-new-version -avoid-location
// RUN: %api-digester -diagnose-sdk -print-module --input-paths %t.dump1.json -input-paths %t.dump2.json -o %t.result

// RUN: %clang -E -P -x c %S/Outputs/Foo-diff.txt -o - | sed '/^\s*$/d' > %t.expected
// RUN: %clang -E -P -x c %t.result -o - | sed '/^\s*$/d' > %t.result.tmp
// RUN: diff -u %t.expected %t.result.tmp
