var x = 10

// REQUIRES: objc_interop
// RUN: rm -rf %t.overlays
// RUN: mkdir %t.overlays

// RUN: %swift -emit-module -o %t.overlays -F %S/../Inputs/libIDE-mock-sdk %S/../Inputs/libIDE-mock-sdk/Mixed.swift -import-underlying-module -module-name Mixed -disable-objc-attr-requires-foundation-module
// RUN: %sourcekitd-test -req=interface-gen -module Mixed -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %clang-importer-sdk | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: PureSwiftClass
