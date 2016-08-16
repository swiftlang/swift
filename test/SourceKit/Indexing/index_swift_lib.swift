// REQUIRES: FIXME

// RUN: %sourcekitd-test -req=index %swiftlib_dir/macosx/x86_64/Swift.swiftmodule -- %mcp_opt > %t.txt
// RUN: %FileCheck %s -input-file %t.txt

// CHECK:      key.name: "Dictionary",
// CHECK-NEXT: key.usr: "s:Vs10Dictionary",

// RUN: %FileCheck %s -input-file %t.txt -check-prefix=CHECK-INTERNAL
// RUN: %sourcekitd-test -req=index %swiftlib_dir/macosx/x86_64/Foundation.swiftmodule -- %clang-importer-sdk %mcp_opt > %t.foundation.txt
// RUN: %FileCheck %s -input-file %t.foundation.txt -check-prefix=CHECK-FOUNDATION

// CHECK-INTERNAL-NOT: key.name: "_bridgeToObjectiveCUnconditional",

// CHECK-FOUNDATION-NOT: _convertStringToNSString
