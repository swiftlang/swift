// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 -g -verify-debug-info -o verify-debug-info %s 2>&1 | %FileCheck  %s -check-prefix=VERIFY-DEBUG-INFO

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 -gdwarf-types -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefix=VERIFY-DEBUG-INFO

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.10 -gline-tables-only -verify-debug-info %s 2>&1 | %FileCheck %s -check-prefix=VERIFY-DEBUG-INFO

// VERIFY-DEBUG-INFO: dsymutil{{(\.exe)?}}{{"?}} verify-debug-info -o verify-debug-info.dSYM
// VERIFY-DEBUG-INFO: dwarfdump --verify --debug-info --eh-frame --quiet verify-debug-info.dSYM
