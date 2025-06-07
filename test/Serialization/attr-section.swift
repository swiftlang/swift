// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_SymbolLinkageMarkers

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -enable-experimental-feature SymbolLinkageMarkers -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// BC-CHECK: <Section_DECL_ATTR

// MODULE-CHECK: @_section("SOME_SECT") let Constant: Int
@_section("SOME_SECT")
let Constant = 321
