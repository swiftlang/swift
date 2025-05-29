// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: echo '#include "header-to-print-availability.h"' > %t.m
// RUN: cp %S/Inputs/print_clang_header/header-to-print-availability.h %t/
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %t/header-to-print-availability.h --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %t > %t.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print-availability.h.printed.txt %t.txt

// RUN: echo '@import HeaderToPrintAvailability;' > %t.module.m
// Test header interface printing from a clang module.
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -source-filename %s -print-header -header-to-print %S/Inputs/print_clang_header/header-to-print-availability.h --cc-args %target-cc-options -isysroot %clang-importer-sdk-path -fsyntax-only %t.module.m -I %S/Inputs/print_clang_header > %t.module.txt
// RUN: diff -u %S/Inputs/print_clang_header/header-to-print-availability.h.module.printed.txt %t.module.txt
