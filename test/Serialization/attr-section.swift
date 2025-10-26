// REQUIRES: swift_in_compiler

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module-path %t/a.swiftmodule -module-name a %s
// RUN: llvm-bcanalyzer -dump %t/a.swiftmodule | %FileCheck --check-prefix BC-CHECK --implicit-check-not UnknownCode %s
// RUN: %target-swift-ide-test -print-module -module-to-print a -source-filename x -I %t | %FileCheck --check-prefix MODULE-CHECK %s

// BC-CHECK: <Section_DECL_ATTR

// MODULE-CHECK: @section("SOME_SECT") let Constant: Int
@section("SOME_SECT")
let Constant = 321
