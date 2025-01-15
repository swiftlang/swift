// Tests that `#fileID` has the correct module name when a module aliases
// itself (to internally refer to itself a raw identifier name instead of the
// -module-name, which must be filesystem-friendly).

// RUN: %target-swift-frontend -module-name Original -module-alias "^raw*identifier^=Original" %s -emit-silgen | %FileCheck %s

print("#fileID = \(#fileID)")
// CHECK:     %{{[0-9]+}} = string_literal utf8 "`^raw*identifier^`/fileid-raw-identifier-module-name.swift"
// CHECK-NOT: %{{[0-9]+}} = string_literal utf8 "Original/fileid-raw-identifier-module-name.swift"
