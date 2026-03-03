// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)

// RUN: %api-digester -dump-sdk -module XccTest -o %t.with-define.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %S/Inputs/XccTest -Xcc -DMY_MACRO
// RUN: %api-digester -dump-sdk -module XccTest -o %t.without-define.json -module-cache-path %t.module-cache %clang-importer-sdk-nosource -I %S/Inputs/XccTest

// RUN: grep -q "Hidden" %t.with-define.json
// RUN: grep -q "Exposed" %t.with-define.json

// RUN: not grep -q "Hidden" %t.without-define.json
// RUN: grep -q "Exposed" %t.without-define.json