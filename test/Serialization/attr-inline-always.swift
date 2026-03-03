// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature InlineAlways -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// REQUIRES: swift_feature_InlineAlways

// BC-CHECK: <Inline_DECL_ATTR

// MODULE-CHECK: @inline(always) func inlineAlwaysFunc()

@inline(always)
public func inlineAlwaysFunc() {}
