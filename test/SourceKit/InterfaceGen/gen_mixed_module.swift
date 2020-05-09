var x = 10

// REQUIRES: objc_interop

// FIXME: the test output we're comparing to is specific to macOS.
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t.overlays)
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend -emit-module -o %t.overlays -F %S/../Inputs/libIDE-mock-sdk %S/../Inputs/libIDE-mock-sdk/Mixed.swift -import-underlying-module -module-name Mixed -disable-objc-attr-requires-foundation-module
// RUN: %sourcekitd-test -req=interface-gen -module Mixed -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: PureSwiftClass
