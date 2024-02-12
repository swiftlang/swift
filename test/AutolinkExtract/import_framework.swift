// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -c %s -F %S/Inputs/Frameworks -o %t/import_framework.o
// RUN: %target-swift-autolink-extract %t/import_framework.o -o - | %FileCheck --check-prefix CHECK-%target-object-format %s

// REQUIRES: autolink-extract

// CHECK-elf-NOT: Link
// CHECK-coff-NOT: Link

import Link
_ = Link.APIFromLinkFramework
