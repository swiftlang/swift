// REQUIRES: FIXME

// RUN: %sourcekitd-test -req=interface-gen -module ObjectiveC.NSObject -- %mcp_opt %clang-importer-sdk > %t.response
// RUN: %FileCheck -check-prefix=CHECK-DESCRIPTION -input-file %t.response %s

// CHECK-DESCRIPTION: var description: String
