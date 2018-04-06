// REQUIRES: OS=macosx
// RUN: %empty-directory(%t.mod)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t.module-cache)
// RUN: %api-digester -dump-sdk -module Foo -o %t.result -module-cache-path %t.module-cache %clang-importer-sdk-nosource -swift-version 4 -I %S/Inputs/Foo -avoid-location
// RUN: diff -u %S/Outputs/clang-module-dump.txt %t.result