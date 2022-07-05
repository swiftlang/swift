// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -emit-module -emit-module-path %t/empty.swiftmodule -module-name empty -module-link-name empty %S/empty.swift
// RUN: %target-swiftc_driver -c %s -I %t -o %t/import_experimental.o
// RUN: %target-swift-autolink-extract %t/import_experimental.o -o - | %FileCheck --check-prefix CHECK-%target-object-format %s
// RUN: %target-swiftc_driver -c %s -I %t -o %t/import_experimental_again.o
// RUN: %target-swift-autolink-extract %t/import_experimental.o %t/import_experimental_again.o -o - | %FileCheck --check-prefix CHECK-%target-object-format %s
// RUN: %target-swift-autolink-extract %t/import_experimental.o %t/import_experimental_again.o -o - | %FileCheck --check-prefix UNIQUE %s

// REQUIRES: autolink-extract

// UNIQUE-COUNT-1: -lswiftCore

// CHECK-elf-DAG: -lswiftCore
// CHECK-elf-DAG: -lempty

// CHECK-coff-DAG: -lswiftCore
// CHECK-coff-DAG: -lempty

import empty
