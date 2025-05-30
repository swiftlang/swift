// REQUIRES: swift_feature_SafeInteropWrappers

// RN: %target-swift-ide-test -print-module -module-to-print=ClangIncludesNoExportModule -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s

// swift-ide-test doesn't currently typecheck the macro expansions, so run the compiler as well
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesNoExport.swiftmodule -I %S/Inputs -enable-experimental-feature SafeInteropWrappers %s

import ClangIncludesNoExportModule

public func callBasicInclude(_ p: Span<CInt>) {
  basic_include(p)
}

public func callNonExported(_ p: Span<CInt>) {
  non_exported_include(p)
}

public func callSubmoduleInclude(_ p: Span<CInt>) {
  submodule_include(p)
}
