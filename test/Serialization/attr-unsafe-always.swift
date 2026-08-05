// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: %llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// BC-CHECK: <Unsafe_DECL_ATTR

// MODULE-CHECK: @unsafe(always) func alwaysUnsafeFunc()
// MODULE-CHECK: @unsafe func unsafeFunc()

@unsafe(always)
public func alwaysUnsafeFunc() {}

@unsafe
public func unsafeFunc() {}
